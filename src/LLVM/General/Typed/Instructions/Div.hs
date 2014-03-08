{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.Instructions.Div
  ( CanDiv
  , Div
  , LLVM.General.Typed.Instructions.Div.div
  ) where

import Data.Bits
import GHC.Exts (Constraint)
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.ValueJoin
import LLVM.General.Typed.VMap

class Div (classification :: Classification) where
  -- nasty :(
  type DivConstraint classification a :: Constraint
  type DivConstraint classification a = ()
  vdiv
    :: (ClassificationOf (Value (cx `Weakest` cy) a) ~ classification, DivConstraint classification a)
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a

instance Div 'IntegerClass where
  type DivConstraint 'IntegerClass a = Bits a
  vdiv
    :: forall cx a cy . (ClassificationOf (Value (cx `Weakest` cy) a) ~ 'IntegerClass, DivConstraint 'IntegerClass a)
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a
  vdiv = vmap2 f g where
    si = isSigned (undefined :: a)
    (cf, gf) = if si then (Constant.SDiv, AST.SDiv) else (Constant.UDiv, AST.UDiv)
    f = cf False
    g x y = nameInstruction $ gf False x y []

instance Div 'FloatingPointClass where
 vdiv = vmap2 f g where
   f = Constant.FDiv
   g x y = nameInstruction $ AST.FDiv x y []

type family CanDiv (a :: *) (b :: *) :: Constraint
type instance CanDiv (Value cx a) (Value cy a) = (Div (ClassificationOf (Value (cx `Weakest` cy) a)), DivConstraint (ClassificationOf (Value (cx `Weakest` cy) a)) a)

div
  :: CanDiv (Value cx a) (Value cy a)
  => Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) a)
div x y = vjoin $ vdiv x y
