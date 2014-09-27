{-# LANGUAGE FlexibleContexts #-}

module LLVM.General.Typed.Instructions.Ret
  ( ret
  , ret_
  ) where

import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf

ret
  :: ValueOf (Value const a)
  => Value const a
  -> BasicBlock (Terminator a ())
ret value = do
  -- name the value, emitting instructions as necessary
  valueOp <- asOp value
  setTerminator $ AST.Ret (Just valueOp) []
  -- @TODO: replace with LocalReference ?
  return $ Terminator ()

ret_ :: BasicBlock (Terminator () ())
ret_ = do
  setTerminator $ AST.Ret Nothing []
  return $ Terminator ()
