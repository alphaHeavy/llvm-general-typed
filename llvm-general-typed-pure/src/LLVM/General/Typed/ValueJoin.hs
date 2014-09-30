{-# LANGUAGE GADTs #-}

module LLVM.General.Typed.ValueJoin where

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.Value

vjoin :: Value const a -> BasicBlock (Value const a)
vjoin (ValueOperand a) = a >>= return . ValuePure
vjoin a = return a
