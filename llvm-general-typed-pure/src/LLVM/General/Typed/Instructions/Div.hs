{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.Instructions.Div
  ( Div
  , LLVM.General.Typed.Instructions.Div.div
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

idiv
  :: forall const'a const'b a
   . IntegerOf a
  => Value const'a a
  -> Value const'b a
  -> Value (const'a `Weakest` const'b) a
idiv = vmap2 f g where
  si = isSignedInt (Proxy :: Proxy a)
  (cf, gf) = if si then (Constant.SDiv, AST.SDiv) else (Constant.UDiv, AST.UDiv)
  f = cf False
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ gf False x y []

fdiv
  :: forall const'a const'b a
   . ValueOf a
  => Value const'a a
  -> Value const'b a
  -> Value (const'a `Weakest` const'b) a
fdiv = vmap2 f g where
  f = Constant.FDiv
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.FDiv AST.NoFastMathFlags x y []

class Div (classification :: Classification) a where
  vdiv
    :: (ClassificationOf a ~ classification, ValueOf a)
    => Value const'a a
    -> Value const'b a
    -> Value (const'a `Weakest` const'b) a

instance IntegerOf a => Div 'IntegerClass a where
  vdiv = idiv

instance IntegerOf a => Div ('VectorClass 'IntegerClass) a where
  vdiv = idiv

instance Div 'FloatingPointClass a where
  vdiv = fdiv

instance Div ('VectorClass 'FloatingPointClass) a where
  vdiv = fdiv

div
  :: ValueOf a
  => Div (ClassificationOf a) a
  => Value const'a a -- ^ First operand
  -> Value const'b a -- ^ Second operand
  -> BasicBlock (Value (const'a `Weakest` const'b) a) -- ^ Result
div x y = vjoin $ vdiv x y
