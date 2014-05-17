{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Select
  ( select
  ) where

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.VMap

-- the condition constness must match the result constness. this implies that
-- if both true and false values are constant the switch condition must also be
-- a constant. if you want a constant condition but mutable values (for some reason...)
-- just wrap the condition with 'mutable'
select
  :: Value cc Bool
  -> Value ct a
  -> Value cf a
  -> BasicBlock (Value (cc `Weakest` ct `Weakest` cf) a)
select = vmap3' f g where
  f = Constant.Select
  g c t f' = nameInstruction $ AST.Select c t f' []
