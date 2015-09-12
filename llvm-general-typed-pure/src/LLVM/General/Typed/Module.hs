{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.Module
  ( Module
  , evalModule
  , namedModule
  , FunctionType(..)
  , ArgumentList
  , namedFunction
  , namedFunction_
  ) where

import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import Data.Proxy
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.Global as Global

import LLVM.General.Typed.ArgumentList
import LLVM.General.Typed.CallingConv
import LLVM.General.Typed.Function
import LLVM.General.Typed.FunctionDefinition
import LLVM.General.Typed.FunctionType
import LLVM.General.Typed.Value

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
      let defnSt = FunctionDefinitionState
            { functionDefinitionBasicBlocks = []
            , functionDefinitionFreshId = fromIntegral (length argumentTypes)
            , functionDefinitionParameters = [AST.Parameter ty (AST.UnName i) [] | (ty, i) <- zip argumentTypes [0..]]
            }
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
