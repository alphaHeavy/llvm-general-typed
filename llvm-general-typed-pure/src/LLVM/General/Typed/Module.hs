{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.Module
  ( Module
  , evalModule
  , namedModule
  , FunctionType(..)
  , ArgumentList
  , namedFunction
  , namedFunction_
  ) where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import Data.Proxy
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.Global as Global

import LLVM.General.Typed.CallingConv
import LLVM.General.Typed.Function
import LLVM.General.Typed.FunctionDefinition
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf (ValueOf, valueType)

newtype Module a = Module{runModule :: State ModuleState a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState ModuleState)

data ModuleState = ModuleState
  { moduleName        :: String
  , moduleDefinitions :: [AST.Definition]
  }

newtype Globals a = Globals{runGlobals :: State [AST.Global] a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState [AST.Global])

evalModule :: Module a -> (AST.Module, a)
evalModule (Module a) = (m, a') where
  m = AST.Module n Nothing Nothing defs
  n = moduleName st'
  defs = moduleDefinitions st'
  st = ModuleState{moduleName = "unnamed module", moduleDefinitions = []}
  ~(a', st') = runState a st

namedModule :: String -> Globals a -> Module a
namedModule n body = do
  let ~(a, defs) = runState (runGlobals body) []
  st <- get
  put $!  st{moduleName = n, moduleDefinitions = fmap AST.GlobalDefinition defs}
  return a

-- |
-- Convert a function type @ a -> b -> c -> ... @ to a type level list @ [a, b, c, ...] @
-- to support instance matching without a sentinel or enabling IncoherentInstances
type family ArgumentList (args :: *) :: [*] where
  ArgumentList (a -> b) = a ': ArgumentList b
  ArgumentList a = '[a]

class FunctionType (a :: [*]) where
  functionType :: proxy a -> [AST.Type]

instance (ValueOf (Value 'Mutable x), FunctionType xs) => FunctionType (x ': xs) where
  functionType _ = valueType (Proxy :: Proxy (Value 'Mutable x)) : functionType (Proxy :: Proxy xs)

instance FunctionType '[] where
  functionType _ = []

-- |
-- Convert a type list from Haskell format (argType1 -> argType2 -> returnType)
-- to ([argType1, argType2], returnType), matching the format expected by llvm
splitFunctionTypes :: [AST.Type] -> Maybe ([AST.Type], AST.Type)
splitFunctionTypes = go [] where
  go  _     [] = Nothing
  go ys    [x] = Just (reverse ys, x)
  go ys (x:xs) = go (x:ys) xs

namedFunction_
  :: (FunctionType (ArgumentList ty), KnownNat cconv)
  => String
  -> FunctionDefinition ty ()
  -> Globals (Function ('CallingConv cconv) ty)
namedFunction_ n defn = fst <$> namedFunction n defn

namedFunction
  :: forall a cconv ty
   . (FunctionType (ArgumentList ty), KnownNat cconv)
  => String
  -> FunctionDefinition ty a
  -> Globals (Function ('CallingConv cconv) ty, a)
namedFunction n (FunctionDefinition defn) = do
  case splitFunctionTypes (functionType (Proxy :: Proxy (ArgumentList ty))) of
    Nothing -> fail "Empty function types?"
    Just (argumentTypes, returnType) -> do
      let defnSt = FunctionDefinitionState{functionDefinitionBasicBlocks = [], functionDefinitionFreshId = 0, functionDefinitionParameters = []}
          ~(a, defSt') = runState (runFunctionDefinition defn) defnSt
          name = AST.Name n
          params = functionDefinitionParameters defSt'
          ft = AST.FunctionType returnType argumentTypes False
          x = AST.functionDefaults
               { Global.basicBlocks = functionDefinitionBasicBlocks defSt'
               , Global.name = AST.Name n
               , Global.parameters = (params, False)
               , Global.returnType = returnType
               }

      let paramTy = [ty | Global.Parameter ty _ _ <- params]
      when (argumentTypes /= paramTy) $
        fail $ "Parameter type mismatch: " ++ show argumentTypes ++ " /= " ++ show paramTy

      st <- get
      put $! x:st
      return (createFunction (ValueConstant (Constant.GlobalReference ft name)), a)
