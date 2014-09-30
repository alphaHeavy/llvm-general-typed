{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module LLVM.General.Typed.ValueSelect where

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.Value

class ValueSelect (const :: Constness) (const' :: Constness) where
  vselect
    :: (Constant.Constant -> Constant.Constant)
    -> (AST.Operand -> BasicBlock AST.Operand)
    -> Value const a
    -> Value const' b

instance ValueSelect 'Constant 'Constant where
  vselect f _ (ValueConstant v) = ValueConstant (f v)

instance ValueSelect 'Constant 'Mutable where
  vselect _ g (ValueConstant v) = ValueOperand (g (AST.ConstantOperand v))

instance ValueSelect 'Mutable 'Mutable where
  vselect f g (ValueMutable v) = vselect f g v
  vselect _ g (ValuePure v)    = ValueOperand (g v)
  vselect _ g (ValueOperand v) = ValueOperand (g =<< v)
