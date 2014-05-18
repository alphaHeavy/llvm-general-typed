{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.Instructions.Add
  ( CanAdd
  , Add
  , add
  ) where

import GHC.Exts (Constraint)
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.ValueJoin
import LLVM.General.Typed.VMap

iadd
  :: Value cx a
  -> Value cy a
  -> Value (cx `Weakest` cy) a
iadd = vmap2 f g where
  f = Constant.Add False False
  g x y = nameInstruction $ AST.Add False False x y []

fadd
  :: Value cx a
  -> Value cy a
  -> Value (cx `Weakest` cy) a
fadd = vmap2 f g where
  f = Constant.FAdd
  g x y = nameInstruction $ AST.FAdd AST.NoFastMathFlags x y []

class Add (classification :: Classification) where
  vadd
    :: ClassificationOf (Value (cx `Weakest` cy) a) ~ classification
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a

instance Add 'IntegerClass where
  vadd = iadd

instance Add ('VectorClass 'IntegerClass) where
  vadd = iadd

instance Add 'FloatingPointClass where
  vadd = fadd

instance Add ('VectorClass 'FloatingPointClass) where
  vadd = fadd

type family CanAdd (a :: *) (b :: *) :: Constraint
type instance CanAdd (Value cx a) (Value cy a) = Add (ClassificationOf (Value (cx `Weakest` cy) a))

add
  :: CanAdd (Value cx a) (Value cy a)
  => Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) a)
add x y = vjoin $ vadd x y