{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.General.Typed.FunctionDefinition where

import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import qualified LLVM.General.AST as AST

newtype UntypedFunctionDefinition a = UntypedFunctionDefinition{runFunctionDefinition :: State FunctionDefinitionState a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState FunctionDefinitionState)

newtype FunctionDefinition ty a = FunctionDefinition (UntypedFunctionDefinition a)
  deriving (Functor, Applicative, Monad, MonadFix, MonadState FunctionDefinitionState)

data FunctionDefinitionState = FunctionDefinitionState
  { functionDefinitionBasicBlocks :: [AST.BasicBlock]
  , functionDefinitionFreshId     :: {-# UNPACK #-} !Word
  , functionDefinitionParameters  :: [AST.Parameter]
  }
