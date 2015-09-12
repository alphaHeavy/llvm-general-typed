{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Extend
  ( CanExtend
  , ext
  ) where

import Data.Bits
import Data.Proxy
import GHC.Exts (Constraint)
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.VMap

type family CanExtend a b :: Constraint
type instance CanExtend a b = (Bits a, ClassificationOf a ~ 'IntegerClass, ClassificationOf b ~ 'IntegerClass)

ext
  :: forall a b const . (CanExtend a b, BitsOf a + 1 <= BitsOf b)
  => ValueOf b
  => Value const a
  -> BasicBlock (Value const b)
ext = vmap1' f g where
  si = isSigned (undefined :: a)
  (cf, gf) = if si then (Constant.SExt, AST.SExt) else (Constant.ZExt, AST.ZExt)
  vt = valueType (Proxy :: Proxy b)
  f v = cf v vt
  g v = nameInstruction vt $ gf v vt []
