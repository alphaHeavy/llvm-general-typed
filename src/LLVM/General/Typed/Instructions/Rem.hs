{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.Instructions.Rem
  ( CanRem
  , Rem
  , LLVM.General.Typed.Instructions.Rem.rem
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

class Rem (classification :: Classification) where
  -- nasty :(
  type RemConstraint classification a :: Constraint
  type RemConstraint classification a = ()
  vrem
    :: (ClassificationOf (Value (cx `Weakest` cy) a) ~ classification, RemConstraint classification a)
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a

instance Rem 'IntegerClass where
  type RemConstraint 'IntegerClass a = Bits a
  vrem
    :: forall cx a cy . (ClassificationOf (Value (cx `Weakest` cy) a) ~ 'IntegerClass, RemConstraint 'IntegerClass a)
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a
  vrem = vmap2 f g where
    si = isSigned (undefined :: a)
    (f, gf) = if si then (Constant.SRem, AST.SRem) else (Constant.URem, AST.URem)
    g x y = nameInstruction $ gf x y []

instance Rem 'FloatingPointClass where
 vrem = vmap2 f g where
   f = Constant.FRem
   g x y = nameInstruction $ AST.FRem x y []

type family CanRem (a :: *) (b :: *) :: Constraint
type instance CanRem (Value cx a) (Value cy a) = (Rem (ClassificationOf (Value (cx `Weakest` cy) a)), RemConstraint (ClassificationOf (Value (cx `Weakest` cy) a)) a)

rem
  :: CanRem (Value cx a) (Value cy a)
  => Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) a)
rem x y = vjoin $ vrem x y
