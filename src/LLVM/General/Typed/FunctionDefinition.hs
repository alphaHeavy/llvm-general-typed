{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.General.Typed.FunctionDefinition where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import Data.Word
import qualified LLVM.General.AST as AST

newtype FunctionDefinition a = FunctionDefinition{runFunctionDefinition :: State FunctionDefinitionState a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState FunctionDefinitionState)

data FunctionDefinitionState = FunctionDefinitionState
  { functionDefinitionBasicBlocks :: [AST.BasicBlock]
  , functionDefinitionFreshId     :: {-# UNPACK #-} !Word
  , functionDefinitionParameters  :: [AST.Parameter]
  }

