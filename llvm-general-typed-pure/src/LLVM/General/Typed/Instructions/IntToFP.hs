{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module LLVM.General.Typed.Instructions.IntToFP
  ( inttofp
  ) where

import Data.Proxy
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.IntegerOf
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.VMap

-- |
-- Convert an integer to a floating point value.
inttofp
  :: forall a b const
   . (IntegerOf a, ValueOf b)
  => ClassificationOf a ~ 'IntegerClass
  => ClassificationOf b ~ 'FloatingPointClass
  => Value const a -- ^ Source value must be of the 'IntegerClass'
  -> BasicBlock (Value const b) -- ^ Result must be of the 'FloatingPointClass'
inttofp = vmap1' f g where
  si = isSignedInt (Proxy :: Proxy a)
  (cf, gf) = if si then (Constant.SIToFP, AST.SIToFP) else (Constant.UIToFP, AST.UIToFP)
  vt = valueType (Proxy :: Proxy b)
  f v = cf v vt
  g v = nameInstruction vt $ gf v vt []
