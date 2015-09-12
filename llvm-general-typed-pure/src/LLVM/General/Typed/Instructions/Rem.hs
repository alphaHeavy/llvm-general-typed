{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

irem
  :: forall a cy cx
   . (Bits a, ValueOf a)
  => Value cx a
  -> Value cy a
  -> Value (cx `Weakest` cy) a
irem = vmap2 f g where
  si = isSigned (undefined :: a)
  (f, gf) = if si then (Constant.SRem, AST.SRem) else (Constant.URem, AST.URem)
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ gf x y []

frem
  :: forall a cy cx
   . ValueOf a
  => Value cx a
  -> Value cy a
  -> Value (cx `Weakest` cy) a
frem = vmap2 f g where
  f = Constant.FRem
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.FRem AST.NoFastMathFlags x y []

class Rem (classification :: Classification) where
  -- nasty :(
  type RemConstraint classification a :: Constraint
  type RemConstraint classification a = ()
  vrem
    :: (ClassificationOf a ~ classification, RemConstraint classification a, ValueOf a)
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a

instance Rem 'IntegerClass where
  type RemConstraint 'IntegerClass a = Bits a
  vrem = irem

instance Rem ('VectorClass 'IntegerClass) where
  type RemConstraint ('VectorClass 'IntegerClass) a = Bits a
  vrem = irem

instance Rem ('VectorClass 'FloatingPointClass) where
  vrem = frem

instance Rem 'FloatingPointClass where
  vrem = frem

type family CanRem (a :: *) (b :: *) :: Constraint
type instance CanRem a a = (Rem (ClassificationOf a), RemConstraint (ClassificationOf a) a, ValueOf a)

rem
  :: CanRem a a
  => Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) a)
rem x y = vjoin $ vrem x y
