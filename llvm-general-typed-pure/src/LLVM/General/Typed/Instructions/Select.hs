{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Select
  ( select
  ) where

import Data.Proxy
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.VMap

-- |
-- Select one of two values, depending on a Boolean condition.
select
  :: forall a cc ct cf
   . ValueOf a
  => Value cc Bool -- ^ Condition
  -> Value ct a -- ^ First operand
  -> Value cf a -- ^ Second operand
  -> BasicBlock (Value (cc `Weakest` ct `Weakest` cf) a) -- ^ Result
select = vmap3' f g where
  f = Constant.Select
  ty = valueType (Proxy :: Proxy a)
  g c t f' = nameInstruction ty $ AST.Select c t f' []
