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
type instance CanExtend (Value const a) (Value const b) = (Bits a, ClassificationOf (Value const a) ~ IntegerClass, ClassificationOf (Value const b) ~ IntegerClass)

ext
  :: forall a b const . (CanExtend (Value const a) (Value const b), BitsOf (Value const a) + 1 <= BitsOf (Value const b))
  => ValueOf (Value const b)
  => Value const a
  -> BasicBlock (Value const b)
ext = vmap1' f g where
  si = isSigned (undefined :: a)
  (cf, gf) = if si then (Constant.SExt, AST.SExt) else (Constant.ZExt, AST.ZExt)
  vt = valueType (Proxy :: Proxy (Value const b))
  f v = cf v vt
  g v = nameInstruction vt $ gf v vt []
