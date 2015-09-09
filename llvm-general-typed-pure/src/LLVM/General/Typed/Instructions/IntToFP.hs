{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module LLVM.General.Typed.Instructions.IntToFP
  ( CanIntToFP
  , inttofp
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
import LLVM.General.Typed.VMap

type family CanIntToFP a b :: Constraint
type instance CanIntToFP (Value const a) (Value const b) = (Bits a, ClassificationOf (Value const a) ~ 'IntegerClass, ClassificationOf (Value const b) ~ 'FloatingPointClass)

inttofp :: forall a b const . CanIntToFP (Value const a) (Value const b) => ValueOf (Value const b) => Value const a -> BasicBlock (Value const b)
inttofp = vmap1' f g where
  si = isSigned (undefined :: a)
  (cf, gf) = if si then (Constant.SIToFP, AST.SIToFP) else (Constant.UIToFP, AST.UIToFP)
  vt = valueType (Proxy :: Proxy (Value const b))
  f v = cf v vt
  g v = nameInstruction vt $ gf v vt []
