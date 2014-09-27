{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.ValueWrap where

import Control.Applicative
import Control.Monad
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

instance OperandWrap Mutable where
  operandWrap :: forall a. ValueOf a => AST.Operand -> Maybe (Value 'Mutable a)
  operandWrap op@(AST.LocalReference ty _)
    | valueType (Proxy :: Proxy a) == ty = Just (ValueOperand (return op))
  operandWrap op@AST.ConstantOperand{} = ValueMutable <$> operandWrap op
  operandWrap _ = Nothing

instance OperandWrap Constant where
  operandWrap :: forall a. ValueOf a => AST.Operand -> Maybe (Value 'Constant a)
  operandWrap (AST.ConstantOperand op) = do
    let val = ValueConstant op
    case (op, valueType (Proxy :: Proxy a)) of
      (Constant.Int n _, AST.IntegerType n') | n == n' -> Just val
      (Constant.Float (Float.Single _), AST.FloatingPointType 4 Type.IEEE) -> Just val
      (Constant.Float (Float.Double _), AST.FloatingPointType 8 Type.IEEE) -> Just val
      (Constant.Null typ, rep) | typ == rep -> Just val
      (Constant.Undef typ, rep) | typ == rep -> Just val
      (Constant.Array typ _, rep) | typ == rep -> Just val
      (Constant.GlobalReference typ _, rep) | typ == rep -> Just val
  operandWrap _ = Nothing

functionWrap :: forall a cc . (KnownNat cc, FunctionType (ArgumentList a)) => AST.Global -> Maybe (Function ('CallingConv cc) a)
functionWrap AST.Function{..} = do
  let callingConvention' = reifyCallingConv (Proxy :: Proxy ('CallingConv cc))
  guard (callingConvention == callingConvention')
  (ty', rty) <- splitFunctionTypes $ functionType (Proxy :: Proxy (ArgumentList a))
  guard (returnType == rty)
  let pty = [ty | AST.Parameter ty _ _ <- fst parameters]
      fty = Type.FunctionType returnType pty (snd parameters)
  guard (pty == ty')
  return $ Function (ValueConstant (Constant.GlobalReference fty name)) callingConvention'
functionWrap _ = Nothing
