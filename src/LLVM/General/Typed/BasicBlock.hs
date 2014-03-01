{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.General.Typed.BasicBlock where

import Control.Applicative
import Control.Monad.Fix
import Control.Monad.RWS.Lazy
import Data.Maybe (fromJust)
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.FunctionDefinition

newtype BasicBlock a = BasicBlock{runBasicBlock :: RWST () [AST.Named AST.Instruction] BasicBlockState FunctionDefinition a}
  deriving (Functor, Applicative, Monad, MonadFix, MonadState BasicBlockState, MonadWriter [AST.Named AST.Instruction])

liftFunctionDefinition :: FunctionDefinition a -> BasicBlock a
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

evalBasicBlock :: AST.Name -> BasicBlock (Terminator a) -> FunctionDefinition (a, AST.BasicBlock)
evalBasicBlock n bb = do
  -- pattern match must be lazy to support the MonadFix instance
  ~(Terminator a, st, instr) <- runRWST (runBasicBlock bb) () (BasicBlockState n Nothing)
  return (a, AST.BasicBlock (basicBlockName st) instr (fromJust (basicBlockTerminator st)))
