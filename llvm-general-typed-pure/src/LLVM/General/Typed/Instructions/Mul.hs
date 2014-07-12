{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.Instructions.Mul
  ( CanMul
  , Mul
  , mul
  ) where

import Data.Proxy
import GHC.Exts (Constraint)
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.ValueJoin
import LLVM.General.Typed.VMap

imul
  :: forall a cx cy
   . ValueOf (Value (cx `Weakest` cy) a)
  => Value cx a
  -> Value cy a
  -> Value (cx `Weakest` cy) a
imul = vmap2 f g where
  f = Constant.Mul False False
  ty = valueType (Proxy :: Proxy (Value (cx `Weakest` cy) a))
  g x y = nameInstruction ty $ AST.Mul False False x y []

fmul
  :: forall a cx cy
   . ValueOf (Value (cx `Weakest` cy) a)
  => Value cx a
  -> Value cy a
  -> Value (cx `Weakest` cy) a
fmul = vmap2 f g where
  f = Constant.FMul
  ty = valueType (Proxy :: Proxy (Value (cx `Weakest` cy) a))
  g x y = nameInstruction ty $ AST.FMul AST.NoFastMathFlags x y []

class Mul (classification :: Classification) where
  vmul
    :: (ClassificationOf (Value (cx `Weakest` cy) a) ~ classification, ValueOf (Value (cx `Weakest` cy) a))
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a

instance Mul 'IntegerClass where
  vmul = imul

instance Mul ('VectorClass 'IntegerClass) where
  vmul = imul

instance Mul 'FloatingPointClass where
  vmul = fmul

instance Mul ('VectorClass 'FloatingPointClass) where
  vmul = fmul

type family CanMul (a :: *) (b :: *) :: Constraint
type instance CanMul (Value cx a) (Value cy a) = (Mul (ClassificationOf (Value (cx `Weakest` cy) a)), ValueOf (Value (cx `Weakest` cy) a))

mul
  :: CanMul (Value cx a) (Value cy a)
  => Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) a)
mul x y = vjoin $ vmul x y
