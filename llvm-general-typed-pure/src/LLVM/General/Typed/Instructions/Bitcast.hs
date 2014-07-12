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

bitcast
  :: forall a b const .
     ( BitsOf (Value const a) ~ BitsOf (Value const b)
     , ValueOf (Value const b))
  => Value const a
  -> BasicBlock (Value const b)
bitcast = vmap1' f g where
  vt = valueType (Proxy :: Proxy (Value const b))
  f v = Constant.BitCast v vt
  g v = nameInstruction vt $ AST.BitCast v vt []
