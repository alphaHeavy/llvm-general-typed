{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module LLVM.General.Typed.Instructions.FPToInt
  ( fptoint
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
-- Convert a floating point to an integer value.
fptoint
  :: forall a b const
   . (ValueOf a, IntegerOf b)
  => ClassificationOf a ~ 'FloatingPointClass
  => ClassificationOf b ~ 'IntegerClass
  => Value const a -- ^ Source value must be of the 'FloatingPointClass'
  -> BasicBlock (Value const b) -- ^ Result must be of the 'IntegerClass'
fptoint = vmap1' f g where
  si = isSignedInt (Proxy :: Proxy b)
  (cf, gf) = if si then (Constant.FPToSI, AST.FPToSI) else (Constant.FPToUI, AST.FPToUI)
  vt = valueType (Proxy :: Proxy b)
  f v = cf v vt
  g v = nameInstruction vt $ gf v vt []
