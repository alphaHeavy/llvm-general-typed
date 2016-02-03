{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Trunc
  ( Trunc
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
    :: ( ClassificationOf a ~ classification
       , ClassificationOf b ~ classification
       , ValueOf b
       , BitsOf b + 1 <= BitsOf a)
    => Value const a
    -> BasicBlock (Value const b)

instance Trunc 'IntegerClass where
  vtrunc
    :: forall const a b
     . ( ClassificationOf a ~ 'IntegerClass
       , ClassificationOf b ~ 'IntegerClass
       , ValueOf b
       , BitsOf b + 1 <= BitsOf a)
    => Value const a
    -> BasicBlock (Value const b)
  vtrunc = vmap1' f g where
    vt = valueType (Proxy :: Proxy b)
    f v = Constant.Trunc v vt
    g v = nameInstruction vt $ AST.Trunc v vt []

instance Trunc 'FloatingPointClass where
  vtrunc
    :: forall const a b
     . ( ClassificationOf a ~ 'FloatingPointClass
       , ClassificationOf b ~ 'FloatingPointClass
       , ValueOf b
       , BitsOf b + 1 <= BitsOf a)
    => Value const a
    -> BasicBlock (Value const b)
  vtrunc = vmap1' f g where
    vt = valueType (Proxy :: Proxy b)
    f v = Constant.FPTrunc v vt
    g v = nameInstruction vt $ AST.FPTrunc v vt []

-- |
-- Truncate a value.
trunc
  :: (BitsOf b + 1 <= BitsOf a)
  => ClassificationOf a ~ ClassificationOf b
  => Trunc (ClassificationOf b)
  => ValueOf b
  => Value const a -- ^ Source value, must be wider than the result
  -> BasicBlock (Value const b) -- ^ Result
trunc = vtrunc
