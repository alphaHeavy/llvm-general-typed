{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Num where

import Data.Int
import Data.Word
import GHC.TypeLits

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.Float as Float
import qualified LLVM.General.AST.FloatingPointPredicate as FloatingPointPredicate
import qualified LLVM.General.AST.IntegerPredicate as IntegerPredicate

import BasicBlock
import FreshName
import Instructions
import Value
import ValueOf
import VMap

signumSigned
  :: forall const a .
     ( SingI (BitsOf (Value const a))
     , ClassificationOf (Value const a) ~ IntegerClass
     , Num (Value const a))
  => Value const a
  -> Value const a
signumSigned v =
  case v of
    x@ValueConstant{} -> evalConstantBasicBlock (f x)
    x@ValueMutable{}  -> ValueOperand (f x >>= asOp)
    x@ValueOperand{}  -> ValueOperand (f x >>= asOp)
 where
  f :: (ValueJoin const, Weakest const const ~ const)
    => Value const a
    -> BasicBlock (Value const a)
  f x = do
    gt <- icmp IntegerPredicate.SGT x (0 :: Value const a)
    lt <- icmp IntegerPredicate.SLT x (0 :: Value const a)
    il <- select lt (-1 :: Value const a) (0 :: Value const a)
    ig <- select gt ( 1 :: Value const a) il
    return ig

signumUnsigned
  :: forall const a .
     ( SingI (BitsOf (Value const a))
     , ClassificationOf (Value const a) ~ IntegerClass
     , Num (Value const a))
  => Value const a
  -> Value const a
signumUnsigned v =
  case v of
    x@ValueConstant{} -> evalConstantBasicBlock (f x)
    x@ValueMutable{}  -> ValueOperand (f x >>= asOp)
    x@ValueOperand{}  -> ValueOperand (f x >>= asOp)
 where
  f :: (ValueJoin const, Weakest const const ~ const)
    => Value const a
    -> BasicBlock (Value const a)
  f x = do
    gt <- icmp IntegerPredicate.UGT x (0 :: Value const a)
    select gt (1 :: Value const a) (0 :: Value const a)

signumFloating
  :: forall const a .
     ( SingI (BitsOf (Value const a))
     , ClassificationOf (Value const a) ~ FloatingPointClass
     , Num (Value const a))
  => Value const a
  -> Value const a
signumFloating v =
  case v of
    x@ValueConstant{} -> evalConstantBasicBlock (f x)
    x@ValueMutable{}  -> ValueOperand (f x >>= asOp)
    x@ValueOperand{}  -> ValueOperand (f x >>= asOp)
 where
  f :: (ValueJoin const, Weakest const const ~ const)
    => Value const a
    -> BasicBlock (Value const a)
  f x = do
    gt <- fcmp FloatingPointPredicate.OGT x (0 :: Value const a)
    lt <- fcmp FloatingPointPredicate.OLT x (0 :: Value const a)
    il <- select lt (-1 :: Value const a) (0 :: Value const a)
    select gt ( 1 :: Value const a) il

absSigned
  :: forall const a .
     ( SingI (BitsOf (Value const a))
     , ClassificationOf (Value const a) ~ IntegerClass
     , Num (Value const a))
  => Value const a
  -> Value const a
absSigned v = do
  case v of
    x@ValueConstant{} -> evalConstantBasicBlock (f x)
    x@ValueMutable{}  -> ValueOperand (f x >>= asOp)
    x@ValueOperand{}  -> ValueOperand (f x >>= asOp)
 where
  f :: (ValueJoin const, Weakest const const ~ const)
    => Value const a
    -> BasicBlock (Value const a)
  f x = do
    gt <- icmp IntegerPredicate.SGT x (0 :: Value const a)
    select gt (0 - x) x

absFloating
  :: forall const a .
     ( SingI (BitsOf (Value const a))
     , ClassificationOf (Value const a) ~ FloatingPointClass
     , Num (Value const a))
  => Value const a
  -> Value const a
absFloating v = do
  case v of
    x@ValueConstant{} -> evalConstantBasicBlock (f x)
    x@ValueMutable{}  -> ValueOperand (f x >>= asOp)
    x@ValueOperand{}  -> ValueOperand (f x >>= asOp)
 where
  f :: (ValueJoin const, Weakest const const ~ const)
    => Value const a
    -> BasicBlock (Value const a)
  f x = do
    gt <- fcmp FloatingPointPredicate.OGT x (0 :: Value const a)
    select gt (0 - x) x

fromIntegerConst
  :: forall a const . (SingI (BitsOf (Value const a)), InjectConstant const)
  => Integer
  -> Value const a
fromIntegerConst = injectConstant . Constant.Int bits where
  bits = fromIntegral $ fromSing (sing :: Sing (BitsOf (Value const a)))

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Float) where
  fromInteger = injectConstant . Constant.Float . Float.Single . fromIntegral
  abs = absFloating
  (+) = vmap2 Constant.FAdd (nameInstruction2 AST.FAdd)
  (-) = vmap2 Constant.FSub (nameInstruction2 AST.FSub)
  (*) = vmap2 Constant.FMul (nameInstruction2 AST.FMul)
  signum = signumFloating

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Double) where
  fromInteger = injectConstant . Constant.Float . Float.Double . fromIntegral
  abs = absFloating
  (+) = vmap2 Constant.FAdd (nameInstruction2 AST.FAdd)
  (-) = vmap2 Constant.FSub (nameInstruction2 AST.FSub)
  (*) = vmap2 Constant.FMul (nameInstruction2 AST.FMul)
  signum = signumFloating

instance (InjectConstant const, Weakest const const ~ const, Num (Value const Float)) => Fractional (Value const Float) where
  fromRational = injectConstant . Constant.Float . Float.Single . fromRational
  (/) = vmap2 Constant.FDiv (nameInstruction2 AST.FDiv)

instance (InjectConstant const, Weakest const const ~ const, Num (Value const Double)) => Fractional (Value const Double) where
  fromRational = injectConstant . Constant.Float . Float.Double . fromRational
  (/) = vmap2 Constant.FDiv (nameInstruction2 AST.FDiv)

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int8) where
  fromInteger = fromIntegerConst
  abs = absSigned
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  signum = signumSigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int16) where
  fromInteger = fromIntegerConst
  abs = absSigned
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  signum = signumSigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int32) where
  fromInteger = fromIntegerConst
  abs = absSigned
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  signum = signumSigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Int64) where
  fromInteger = fromIntegerConst
  abs = absSigned
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  signum = signumSigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word8) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  signum = signumUnsigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word16) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  signum = signumUnsigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word32) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  signum = signumUnsigned

instance (InjectConstant const, Weakest const const ~ const) => Num (Value const Word64) where
  fromInteger = fromIntegerConst
  abs = id
  (+) = vmap2 (Constant.Add False False) (nameInstruction2 (AST.Add False False))
  (-) = vmap2 (Constant.Sub False False) (nameInstruction2 (AST.Sub False False))
  (*) = vmap2 (Constant.Mul False False) (nameInstruction2 (AST.Mul False False))
  signum = signumUnsigned
