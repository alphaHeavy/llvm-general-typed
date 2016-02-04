{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.General.Typed.BasicBlock where

import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Data.Maybe (fromJust)
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.FunctionDefinition

newtype BasicBlock a = BasicBlock{unBasicBlock :: RWST () [AST.Named AST.Instruction] BasicBlockState UntypedFunctionDefinition a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState BasicBlockState, MonadWriter [AST.Named AST.Instruction])

-- |
-- Enable defining a new 'Function' by lifting the definition to the parent 'Module'.
liftFunctionDefinition
  :: UntypedFunctionDefinition a -- ^ A function wrapped in an existential
  -> BasicBlock a
liftFunctionDefinition = BasicBlock . lift

data BasicBlockState = BasicBlockState
  { basicBlockName         :: AST.Name
  , basicBlockTerminator   :: Maybe (AST.Named AST.Terminator)
  } deriving (Show)

-- |
-- Set the terminating instruction for this 'BasicBlock'.
setTerminator :: AST.Terminator -> BasicBlock ()
setTerminator term = do
  st <- get
  put $! st{basicBlockTerminator = Just (AST.Do term)}

-- |
-- A named label with a specific return type within a 'BasicBlock'.
data Label rty = Label AST.Name

-- |
-- A named label within a 'BasicBlock'.
data SomeLabel where
  SomeLabel :: Label rty -> SomeLabel

newtype Terminator rty a = Terminator a deriving (Functor, Show)

instance Applicative (Terminator rty) where
  pure = Terminator
  Terminator f <*> x = f <$> x

runBasicBlock :: AST.Name -> BasicBlock (Terminator rty a) -> UntypedFunctionDefinition (AST.BasicBlock, a)
runBasicBlock n bb = do
  -- pattern match must be lazy to support the MonadFix instance
  ~(Terminator a, st, instr) <- runRWST (unBasicBlock bb) () (BasicBlockState n Nothing)
  return (AST.BasicBlock (basicBlockName st) instr (fromJust (basicBlockTerminator st)), a)
