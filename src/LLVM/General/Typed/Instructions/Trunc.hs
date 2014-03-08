{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Trunc
  ( CanTrunc
  , Trunc
  , trunc
  ) where

import Data.Proxy
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.VMap

class Trunc (classification :: Classification) where
  vtrunc
    :: ( ClassificationOf (Value const a) ~ classification
       , ClassificationOf (Value const b) ~ classification
       , ValueOf (Value const b)
       , BitsOf (Value const b) + 1 <= BitsOf (Value const a))
    => Value const a
    -> BasicBlock (Value const b)

instance Trunc IntegerClass where
  vtrunc
    :: forall const a b
     . ( ClassificationOf (Value const a) ~ IntegerClass
       , ClassificationOf (Value const b) ~ IntegerClass
       , ValueOf (Value const b)
       , BitsOf (Value const b) + 1 <= BitsOf (Value const a))
    => Value const a
    -> BasicBlock (Value const b)
  vtrunc = vmap1' f g where
    vt = valueType (Proxy :: Proxy (Value const b))
    f v = Constant.Trunc v vt
    g v = nameInstruction $ AST.Trunc v vt []

instance Trunc FloatingPointClass where
  vtrunc
    :: forall const a b
     . ( ClassificationOf (Value const a) ~ FloatingPointClass
       , ClassificationOf (Value const b) ~ FloatingPointClass
       , ValueOf (Value const b)
       , BitsOf (Value const b) + 1 <= BitsOf (Value const a))
    => Value const a
    -> BasicBlock (Value const b)
  vtrunc = vmap1' f g where
    vt = valueType (Proxy :: Proxy (Value const b))
    f v = Constant.FPTrunc v vt
    g v = nameInstruction $ AST.FPTrunc v vt []

type CanTrunc a b = (ClassificationOf a ~ ClassificationOf b, Trunc (ClassificationOf b), ValueOf b)

trunc
  :: ( CanTrunc (Value const a) (Value const b)
     , BitsOf (Value const b) + 1 <= BitsOf (Value const a))
  => Value const a
  -> BasicBlock (Value const b)
trunc = vtrunc
