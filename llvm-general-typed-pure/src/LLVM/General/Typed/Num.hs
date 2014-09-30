{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
-- 'Prelude.Num' implementations for 'LLVM.General.Typed.Value'
module LLVM.General.Typed.Num
  (
  ) where

import Data.Int
import Data.Proxy
import Data.Word
import GHC.Exts (Constraint)
import GHC.TypeLits

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.Float as Float
import qualified LLVM.General.AST.FloatingPointPredicate as FloatingPointPredicate
import qualified LLVM.General.AST.IntegerPredicate as IntegerPredicate

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Instructions
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.VMap

type family NumConstraints (value :: *) (classOf :: Classification) :: Constraint
type instance NumConstraints (Value const a) classOf =
  ( KnownNat (BitsOf (Value const a))
  , ValueOf (Value const a)
  , ClassificationOf (Value const a) ~ classOf
  , Weakest const const ~ const
  , Num (Value const a))

liftValueExpression
  :: (Value const a -> BasicBlock (Value const a))
  -> Value const a
  -> Value const a
liftValueExpression f v =
  case v of
    x@ValueConstant{} -> evalConstantBasicBlock (f x)
    x@ValueMutable{}  -> ValueOperand (f x >>= asOp)
    x@ValueOperand{}  -> ValueOperand (f x >>= asOp)
    x@ValuePure{}     -> ValueOperand (f x >>= asOp)

signumSigned
  :: forall const a
   . NumConstraints (Value const a) IntegerClass
  => Value const a
  -> Value const a
signumSigned = liftValueExpression $ \ x -> do
  gt <- icmp IntegerPredicate.SGT x (0 :: Value const a)
  lt <- icmp IntegerPredicate.SLT x (0 :: Value const a)
  il <- select lt (-1 :: Value const a) (0 :: Value const a)
  select gt (1 :: Value const a) il

signumUnsigned
  :: forall const a
   . NumConstraints (Value const a) IntegerClass
  => Value const a
  -> Value const a
signumUnsigned = liftValueExpression $ \ x -> do
  gt <- icmp IntegerPredicate.UGT x (0 :: Value const a)
  select gt (1 :: Value const a) (0 :: Value const a)

signumFloating
  :: forall const a
   . NumConstraints (Value const a) FloatingPointClass
  => Value const a
  -> Value const a
signumFloating = liftValueExpression $ \ x -> do
  gt <- fcmp FloatingPointPredicate.OGT x (0 :: Value const a)
  lt <- fcmp FloatingPointPredicate.OLT x (0 :: Value const a)
  il <- select lt (-1 :: Value const a) (0 :: Value const a)
  select gt (1 :: Value const a) il

absSigned
  :: forall const a
   . NumConstraints (Value const a) IntegerClass
  => Value const a
  -> Value const a
absSigned = liftValueExpression $ \ x -> do
  gt <- icmp IntegerPredicate.SGT x (0 :: Value const a)
  select gt (0 - x) x

absFloating
  :: forall const a
   . NumConstraints (Value const a) FloatingPointClass
  => Value const a
  -> Value const a
absFloating = liftValueExpression $ \ x -> do
  gt <- fcmp FloatingPointPredicate.OGT x (0 :: Value const a)
  select gt (0 - x) x

fromIntegerConst
  :: forall a const . (KnownNat (BitsOf (Value const a)), InjectConstant const)
  => Integer
  -> Value const a
fromIntegerConst = injectConstant . Constant.Int bits where
  bits = fromIntegral $ natVal (Proxy :: Proxy (BitsOf (Value const a)))

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Float) where
  fromInteger = injectConstant . Constant.Float . Float.Single . fromIntegral
  abs = absFloating
  (+) = vmap2 Constant.FAdd (nameInstruction2 (valueType (Proxy :: Proxy (Value const Float))) (AST.FAdd AST.NoFastMathFlags))
  (-) = vmap2 Constant.FSub (nameInstruction2 (valueType (Proxy :: Proxy (Value const Float))) (AST.FSub AST.NoFastMathFlags))
  (*) = vmap2 Constant.FMul (nameInstruction2 (valueType (Proxy :: Proxy (Value const Float))) (AST.FMul AST.NoFastMathFlags))
  signum = signumFloating

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Double) where
  fromInteger = injectConstant . Constant.Float . Float.Double . fromIntegral
  abs = absFloating
  (+) = vmap2 Constant.FAdd (nameInstruction2 (valueType (Proxy :: Proxy (Value const Double))) (AST.FAdd AST.NoFastMathFlags))
  (-) = vmap2 Constant.FSub (nameInstruction2 (valueType (Proxy :: Proxy (Value const Double))) (AST.FSub AST.NoFastMathFlags))
  (*) = vmap2 Constant.FMul (nameInstruction2 (valueType (Proxy :: Proxy (Value const Double))) (AST.FMul AST.NoFastMathFlags))
  signum = signumFloating

instance (InjectConstant const, Weakest const const ~ const, Num (Value const Float)) => Fractional (Value const Float) where
  fromRational = injectConstant . Constant.Float . Float.Single . fromRational
  (/) = vmap2 Constant.FDiv (nameInstruction2 (valueType (Proxy :: Proxy (Value const Float))) (AST.FDiv AST.NoFastMathFlags))

instance (InjectConstant const, Weakest const const ~ const, Num (Value const Double)) => Fractional (Value const Double) where
  fromRational = injectConstant . Constant.Float . Float.Double . fromRational
  (/) = vmap2 Constant.FDiv (nameInstruction2 (valueType (Proxy :: Proxy (Value const Double))) (AST.FDiv AST.NoFastMathFlags))

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int8) where
  fromInteger = fromIntegerConst
  abs = absSigned
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int8))) (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int8))) (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int8))) (AST.Mul False False))
  signum = signumSigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int16) where
  fromInteger = fromIntegerConst
  abs = absSigned
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int16))) (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int16))) (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int16))) (AST.Mul False False))
  signum = signumSigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int32) where
  fromInteger = fromIntegerConst
  abs = absSigned
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int32))) (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int32))) (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int32))) (AST.Mul False False))
  signum = signumSigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int64) where
  fromInteger = fromIntegerConst
  abs = absSigned
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int64))) (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int64))) (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Int64))) (AST.Mul False False))
  signum = signumSigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word8) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word8))) (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word8))) (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word8))) (AST.Mul False False))
  signum = signumUnsigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word16) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word16))) (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word16))) (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word16))) (AST.Mul False False))
  signum = signumUnsigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word32) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word32))) (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word32))) (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word32))) (AST.Mul False False))
  signum = signumUnsigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word64) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word64))) (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word64))) (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (valueType (Proxy :: Proxy (Value const Word64))) (AST.Mul False False))
  signum = signumUnsigned
