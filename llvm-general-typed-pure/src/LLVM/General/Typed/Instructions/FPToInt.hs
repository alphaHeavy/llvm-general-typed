{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module LLVM.General.Typed.Instructions.FPToInt
  ( CanFPToInt
  , fptoint
  ) where

import Data.Bits
import Data.Proxy
import GHC.Exts (Constraint)
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.VMap

type family CanFPToInt a b :: Constraint
type instance CanFPToInt a b = (Bits b, ClassificationOf a ~ 'FloatingPointClass, ClassificationOf b ~ 'IntegerClass)

fptoint :: forall a b const . CanFPToInt a b => ValueOf b => Value const a -> BasicBlock (Value const b)
fptoint = vmap1' f g where
  si = isSigned (undefined :: b)
  (cf, gf) = if si then (Constant.FPToSI, AST.FPToSI) else (Constant.FPToUI, AST.FPToUI)
  vt = valueType (Proxy :: Proxy b)
  f v = cf v vt
  g v = nameInstruction vt $ gf v vt []
