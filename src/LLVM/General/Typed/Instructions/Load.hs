{-# LANGUAGE DataKinds #-}

module LLVM.General.Typed.Instructions.Load
  ( load
  ) where

import Control.Applicative
import Foreign.Ptr (Ptr)
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value

load
  :: Bool
  -> Value const (Ptr a)
  -> BasicBlock (Value 'Mutable a)
load volatile value = do
  value' <- asOp value
  ValueOperand . return <$> nameInstruction (AST.Load volatile value' Nothing 0 [])
