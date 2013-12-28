{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Module where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Global as Global

import Function
import FunctionDefinition

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
