{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}

module Main where

import Control.Applicative
import Control.Monad
import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import Data.Int
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as Global
import LLVM.General.PrettyPrint (showPretty)

import DefineBasicBlock
import Function
import FunctionDefinition
import Instructions
import Num ()
import Value

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

namedFunction :: String -> FunctionDefinition a -> Globals (Function cconv ty, a)
namedFunction n defn = do
  let defnSt = FunctionDefinitionState{functionDefinitionBasicBlocks = [], functionDefinitionFreshId = 0}
      ~(a, defSt') = runState (runFunctionDefinition defn) defnSt
      x = AST.functionDefaults
           { Global.basicBlocks = functionDefinitionBasicBlocks defSt'
           , Global.name = AST.Name n
           , Global.returnType = AST.IntegerType 8
           }
  st <- get
  put $! x:st
  return (error "foo", a)

externalFunction :: String -> Globals ty
externalFunction = error "externalFunction"

foo :: Module ()
foo = do
  let val :: Value 'Constant Int8
      val = 42 + 9

  namedModule "foo" $ do
    void . namedFunction "bar" $ mdo
      entryBlock <- basicBlock $ do
        br secondBlock

      secondBlock <- namedBasicBlock (AST.Name "second") $ do
        someLocalPtr <- alloca
        store someLocalPtr (99 :: Value 'Constant Int8)
        someLocal <- load someLocalPtr
        x <- val `add` someLocal
        join $ condBr
          <$> cmp someLocal (mutable 99)
          <*> basicBlock (ret $ abs x * someLocal + mutable (val - signum 8))
          <*> basicBlock (br entryBlock)

      return ()

main :: IO ()
main = do
  putStrLn . showPretty . fst $ evalModule foo
