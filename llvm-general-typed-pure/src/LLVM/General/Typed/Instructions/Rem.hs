{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.Instructions.Rem
  ( Rem
  , LLVM.General.Typed.Instructions.Rem.rem
  ) where

import Data.Proxy
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.IntegerOf
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.ValueJoin
import LLVM.General.Typed.VMap

irem
  :: forall a const'b const'a
   . IntegerOf a
  => Value const'a a
  -> Value const'b a
  -> Value (const'a `Weakest` const'b) a
irem = vmap2 f g where
  si = isSignedInt (Proxy :: Proxy a)
  (f, gf) = if si then (Constant.SRem, AST.SRem) else (Constant.URem, AST.URem)
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ gf x y []

frem
  :: forall a const'b const'a
   . ValueOf a
  => Value const'a a
  -> Value const'b a
  -> Value (const'a `Weakest` const'b) a
frem = vmap2 f g where
  f = Constant.FRem
  ty = valueType (Proxy :: Proxy a)
  g x y = nameInstruction ty $ AST.FRem AST.NoFastMathFlags x y []

class Rem (classification :: Classification) a where
  vrem
    :: (ClassificationOf a ~ classification, ValueOf a)
    => Value const'a a
    -> Value const'b a
    -> Value (const'a `Weakest` const'b) a

instance IntegerOf a => Rem 'IntegerClass a where
  vrem = irem

instance IntegerOf a => Rem ('VectorClass 'IntegerClass) a where
  vrem = irem

instance Rem ('VectorClass 'FloatingPointClass) a where
  vrem = frem

instance Rem 'FloatingPointClass a where
  vrem = frem

rem
  :: ValueOf a
  => Rem (ClassificationOf a) a
  => Value const'a a -- ^ First operand
  -> Value const'b a -- ^ Second operand
  -> BasicBlock (Value (const'a `Weakest` const'b) a) -- ^ Result
rem x y = vjoin $ vrem x y
