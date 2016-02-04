{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Extend
  ( Ext
  , ext
  ) where

import Data.Proxy
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.VMap

class Ext (classification :: Classification) a b where
  vext
    :: ClassificationOf a ~ classification
    => ClassificationOf b ~ classification
    => Value const a
    -> BasicBlock (Value const b)

instance ValueOf b => Ext 'FloatingPointClass a b where
  vext = vmap1' f g where
    vt = valueType (Proxy :: Proxy b)
    f v = Constant.FPExt v vt
    g v = nameInstruction vt $ AST.FPExt v vt []

instance (IntegerOf a, IntegerOf b, IsSigned a ~ IsSigned b) => Ext 'IntegerClass a b where
  vext = vmap1' f g where
    si = isSignedInt (Proxy :: Proxy a)
    (cf, gf) = if si then (Constant.SExt, AST.SExt) else (Constant.ZExt, AST.ZExt)
    vt = valueType (Proxy :: Proxy b)
    f v = cf v vt
    g v = nameInstruction vt $ gf v vt []

ext
  :: forall a b const
   . (ValueOf a, ValueOf b)
  => (BitsOf a + 1 <= BitsOf b)
  => ClassificationOf a ~ ClassificationOf b
  => Ext (ClassificationOf a) a b
  => Value const a -- ^ Source value, must be narrower than the result
  -> BasicBlock (Value const b) -- ^ Result
ext = vext
