{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Mul
  ( Mul
  , mul
  ) where

import Data.Proxy
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.ValueJoin
import LLVM.General.Typed.VMap

imul
  :: forall a const'a const'b
   . ValueOf a
  => Value const'a a
  -> Value const'b a
  -> Value (const'a `Weakest` const'b) a
imul = vmap2 f g where
  f = Constant.Mul False False
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.Mul False False x y []

fmul
  :: forall a const'a const'b
   . ValueOf a
  => Value const'a a
  -> Value const'b a
  -> Value (const'a `Weakest` const'b) a
fmul = vmap2 f g where
  f = Constant.FMul
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.FMul AST.NoFastMathFlags x y []

class Mul (classification :: Classification) where
  vmul
    :: (ClassificationOf a ~ classification, ValueOf a)
    => Value const'a a
    -> Value const'b a
    -> Value (const'a `Weakest` const'b) a

instance Mul 'IntegerClass where
  vmul = imul

instance Mul ('VectorClass 'IntegerClass) where
  vmul = imul

instance Mul 'FloatingPointClass where
  vmul = fmul

instance Mul ('VectorClass 'FloatingPointClass) where
  vmul = fmul

mul
  :: ValueOf a
  => Mul (ClassificationOf a)
  => Value const'a a -- ^ First operand
  -> Value const'b a -- ^ Second operand
  -> BasicBlock (Value (const'a `Weakest` const'b) a) -- ^ Result
mul x y = vjoin $ vmul x y
