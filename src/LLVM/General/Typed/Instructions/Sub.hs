{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.Instructions.Sub
  ( CanSub
  , Sub
  , sub
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

class Sub (classification :: Classification) where
  vsub
    :: ClassificationOf (Value (cx `Weakest` cy) a) ~ classification
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a

instance Sub 'IntegerClass where
 vsub = vmap2 f g where
   f = Constant.Sub False False
   g x y = nameInstruction $ AST.Sub False False x y []

instance Sub 'FloatingPointClass where
 vsub = vmap2 f g where
   f = Constant.FSub
   g x y = nameInstruction $ AST.FSub x y []

type family CanSub (a :: *) (b :: *) :: Constraint
type instance CanSub (Value cx a) (Value cy a) = Sub (ClassificationOf (Value (cx `Weakest` cy) a))

sub
  :: CanSub (Value cx a) (Value cy a)
  => Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) a)
sub x y = vjoin $ vsub x y
