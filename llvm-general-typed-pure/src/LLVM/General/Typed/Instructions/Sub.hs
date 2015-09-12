{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.Instructions.Sub
  ( CanSub
  , Sub
  , sub
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

isub
  :: forall a cx cy
   . ValueOf a
  => Value cx a
  -> Value cy a
  -> Value (cx `Weakest` cy) a
isub = vmap2 f g where
  f = Constant.Sub False False
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.Sub False False x y []

fsub
  :: forall a cx cy
   . ValueOf a
  => Value cx a
  -> Value cy a
  -> Value (cx `Weakest` cy) a
fsub = vmap2 f g where
  f = Constant.FSub
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.FSub AST.NoFastMathFlags x y []

class Sub (classification :: Classification) where
  vsub
    :: (ClassificationOf a ~ classification, ValueOf a)
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a

instance Sub 'IntegerClass where
  vsub = isub

instance Sub ('VectorClass 'IntegerClass) where
  vsub = isub

instance Sub 'FloatingPointClass where
  vsub = fsub

instance Sub ('VectorClass 'FloatingPointClass) where
  vsub = fsub

type family CanSub (a :: *) (b :: *) :: Constraint
type instance CanSub a a = (Sub (ClassificationOf a), ValueOf a)

sub
  :: CanSub a a
  => Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) a)
sub x y = vjoin $ vsub x y
