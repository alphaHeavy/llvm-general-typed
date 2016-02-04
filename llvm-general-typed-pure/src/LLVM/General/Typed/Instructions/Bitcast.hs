{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module LLVM.General.Typed.Instructions.Bitcast
  ( bitcast
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
-- Unsafely convert between two types of equal size.
bitcast
  :: forall a b const
   . (ValueOf a, ValueOf b)
  => (BitsOf a ~ BitsOf b)
  => Value const a -- ^ Source value
  -> BasicBlock (Value const b) -- ^ Result
bitcast = vmap1' f g where
  vt = valueType (Proxy :: Proxy b)
  f v = Constant.BitCast v vt
  g v = nameInstruction vt $ AST.BitCast v vt []
