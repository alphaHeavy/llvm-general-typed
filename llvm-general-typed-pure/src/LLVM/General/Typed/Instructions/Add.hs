{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Add
  ( Add
  , add
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

iadd
  :: forall a const'a const'b
   . ValueOf a
  => Value const'a a
  -> Value const'b a
  -> Value (const'a `Weakest` const'b) a
iadd = vmap2 f g where
  f = Constant.Add False False
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.Add False False x y []

fadd
  :: forall a const'a const'b
   . ValueOf a
  => Value const'a a
  -> Value const'b a
  -> Value (const'a `Weakest` const'b) a
fadd = vmap2 f g where
  f = Constant.FAdd
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.FAdd AST.NoFastMathFlags x y []

class Add (classification :: Classification) where
  vadd
    :: (ClassificationOf a ~ classification, ValueOf a)
    => Value const'a a
    -> Value const'b a
    -> Value (const'a `Weakest` const'b) a

instance Add 'IntegerClass where
  vadd = iadd

instance Add ('VectorClass 'IntegerClass) where
  vadd = iadd

instance Add 'FloatingPointClass where
  vadd = fadd

instance Add ('VectorClass 'FloatingPointClass) where
  vadd = fadd

add
  :: ValueOf a
  => Add (ClassificationOf a)
  => Value const'a a -- ^ First operand
  -> Value const'b a -- ^ Second operand
  -> BasicBlock (Value (const'a `Weakest` const'b) a) -- ^ Result
add x y = vjoin $ vadd x y
