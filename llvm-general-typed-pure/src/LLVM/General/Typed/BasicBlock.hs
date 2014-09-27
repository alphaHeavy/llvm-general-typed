{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.General.Typed.BasicBlock where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Data.Maybe (fromJust)
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.FunctionDefinition

newtype BasicBlock a = BasicBlock{unBasicBlock :: RWST () [AST.Named AST.Instruction] BasicBlockState UntypedFunctionDefinition a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState BasicBlockState, MonadWriter [AST.Named AST.Instruction])

liftFunctionDefinition :: UntypedFunctionDefinition a -> BasicBlock a
liftFunctionDefinition = BasicBlock . lift

data BasicBlockState = BasicBlockState
  { basicBlockName         :: AST.Name
  , basicBlockTerminator   :: Maybe (AST.Named AST.Terminator)
  } deriving (Show)

setTerminator :: AST.Terminator -> BasicBlock ()
setTerminator term = do
  st <- get
  put $! st{basicBlockTerminator = Just (AST.Do term)}

data Label = Label AST.Name

newtype Terminator a = Terminator a deriving (Functor, Show)

instance Applicative Terminator where
  pure = Terminator
  Terminator f <*> x = f <$> x

runBasicBlock :: AST.Name -> BasicBlock (Terminator a) -> UntypedFunctionDefinition (AST.BasicBlock, a)
runBasicBlock n bb = do
  -- pattern match must be lazy to support the MonadFix instance
  ~(Terminator a, st, instr) <- runRWST (unBasicBlock bb) () (BasicBlockState n Nothing)
  return (AST.BasicBlock (basicBlockName st) instr (fromJust (basicBlockTerminator st)), a)
