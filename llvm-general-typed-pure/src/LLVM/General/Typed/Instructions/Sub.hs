{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Sub
  ( Sub
  , sub
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

isub
  :: forall a const'a const'b
   . ValueOf a
  => Value const'a a
  -> Value const'b a
  -> Value (const'a `Weakest` const'b) a
isub = vmap2 f g where
  f = Constant.Sub False False
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.Sub False False x y []

fsub
  :: forall a const'a const'b
   . ValueOf a
  => Value const'a a
  -> Value const'b a
  -> Value (const'a `Weakest` const'b) a
fsub = vmap2 f g where
  f = Constant.FSub
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.FSub AST.NoFastMathFlags x y []

class Sub (classification :: Classification) where
  vsub
    :: (ClassificationOf a ~ classification, ValueOf a)
    => Value const'a a
    -> Value const'b a
    -> Value (const'a `Weakest` const'b) a

instance Sub 'IntegerClass where
  vsub = isub

instance Sub ('VectorClass 'IntegerClass) where
  vsub = isub

instance Sub 'FloatingPointClass where
  vsub = fsub

instance Sub ('VectorClass 'FloatingPointClass) where
  vsub = fsub

sub
  :: ValueOf a
  => Sub (ClassificationOf a)
  => Value const'a a -- ^ First operand
  -> Value const'b a -- ^ Second operand
  -> BasicBlock (Value (const'a `Weakest` const'b) a) -- ^ Result
sub x y = vjoin $ vsub x y
