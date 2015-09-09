{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.ValueWrap
  ( operandWrap
  , constantWrap
  , functionWrap
  ) where

import Control.Monad
import qualified Data.Foldable as Foldable
import Data.Typeable
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.Float as Float
import qualified LLVM.General.AST.Global as AST
import qualified LLVM.General.AST.Type as Type

import LLVM.General.Typed.ArgumentList
import LLVM.General.Typed.CallingConv
import LLVM.General.Typed.Function
import LLVM.General.Typed.FunctionType
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf

class OperandWrap const where
  operandWrap :: ValueOf a => AST.Operand -> Maybe (Value const a)

constantWrap :: (OperandWrap const, ValueOf a) => Constant.Constant -> Maybe (Value const a)
constantWrap = operandWrap . AST.ConstantOperand

instance OperandWrap 'Mutable where
  operandWrap :: forall a. ValueOf a => AST.Operand -> Maybe (Value 'Mutable a)
  operandWrap op@(AST.LocalReference ty _)
    | valueType (Proxy :: Proxy a) == ty = Just (ValuePure op)
  operandWrap op@AST.ConstantOperand{} = ValueMutable <$> operandWrap op
  operandWrap _ = Nothing

constantMatch :: AST.Type -> Constant.Constant -> Bool
constantMatch rep = go where
  go = \ case
    Constant.Int n _ | AST.IntegerType n' <- rep, n == n' -> True
    Constant.Float (Float.Single _) | AST.FloatingPointType 4 Type.IEEE <- rep -> True
    Constant.Float (Float.Double _) | AST.FloatingPointType 8 Type.IEEE <- rep -> True
    Constant.Null typ -> typ == rep
    Constant.Array typ _ -> typ == rep
    Constant.Vector typs -> Foldable.all go typs
    Constant.Undef typ -> typ == rep
    Constant.GlobalReference typ _ -> typ == rep
    Constant.Add _ _ lhs rhs -> go lhs && go rhs
    Constant.FAdd lhs rhs -> go lhs && go rhs
    Constant.Sub _ _ lhs rhs -> go lhs && go rhs
    Constant.FSub lhs rhs -> go lhs && go rhs
    Constant.Mul _ _ lhs rhs -> go lhs && go rhs
    Constant.FMul lhs rhs -> go lhs && go rhs
    Constant.UDiv _ lhs rhs -> go lhs && go rhs
    Constant.SDiv _ lhs rhs -> go lhs && go rhs
    Constant.FDiv lhs rhs -> go lhs && go rhs
    Constant.URem lhs rhs -> go lhs && go rhs
    Constant.SRem lhs rhs -> go lhs && go rhs
    Constant.FRem lhs rhs -> go lhs && go rhs
    Constant.Shl _ _ lhs rhs -> go lhs && go rhs
    Constant.LShr _ lhs rhs -> go lhs && go rhs
    Constant.AShr _ lhs rhs -> go lhs && go rhs
    Constant.And lhs rhs -> go lhs && go rhs
    Constant.Or lhs rhs -> go lhs && go rhs
    Constant.Xor lhs rhs -> go lhs && go rhs
    Constant.Trunc _ typ -> typ == rep
    Constant.ZExt _ typ -> typ == rep
    Constant.SExt _ typ -> typ == rep
    Constant.FPToUI _ typ -> typ == rep
    Constant.FPToSI _ typ -> typ == rep
    Constant.UIToFP _ typ -> typ == rep
    Constant.SIToFP _ typ -> typ == rep
    Constant.FPTrunc _ typ -> typ == rep
    Constant.FPExt _ typ -> typ == rep
    Constant.PtrToInt _ typ -> typ == rep
    Constant.IntToPtr _ typ -> typ == rep
    Constant.BitCast _ typ -> typ == rep
    Constant.ICmp _ _ _ -> valueType (Proxy :: Proxy (Value 'Constant Bool)) == rep
    Constant.FCmp _ _ _ -> valueType (Proxy :: Proxy (Value 'Constant Bool)) == rep
    Constant.Select _ lhs rhs -> go lhs && go rhs

instance OperandWrap 'Constant where
  operandWrap :: forall a. ValueOf a => AST.Operand -> Maybe (Value 'Constant a)
  operandWrap (AST.ConstantOperand op) = do
    let rep = valueType (Proxy :: Proxy a)
    guard $ constantMatch rep op
    return $ ValueConstant op
  operandWrap _ = Nothing

functionWrap :: forall a cc . (KnownNat cc, FunctionType (ArgumentList a)) => AST.Global -> Maybe (Function ('CallingConv cc) a)
functionWrap AST.Function{..} = do
  let callingConvention' = reifyCallingConv (Proxy :: Proxy ('CallingConv cc))
  guard (callingConvention == callingConvention')
  (ty', rty) <- splitFunctionTypes $ functionType (Proxy :: Proxy (ArgumentList a))
  guard $ returnType == rty
  let pty = [ty | AST.Parameter ty _ _ <- fst parameters]
      fty = Type.FunctionType returnType pty (snd parameters)
  guard $ pty == ty'
  return $ Function (ValueConstant (Constant.GlobalReference fty name)) callingConvention'
functionWrap _ = Nothing
