{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Value where

import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import BasicBlock
import FunctionDefinition

data Constness = Constant | Mutable

type family Weakest (x :: k) (y :: k) :: k where
  Weakest 'Constant 'Constant = 'Constant
  Weakest x         y         = 'Mutable

data Value (const :: Constness) (a :: *) where
  ValueMutable     :: Value 'Constant a      -> Value 'Mutable a
  ValueOperand     :: BasicBlock AST.Operand -> Value 'Mutable a
  ValueConstant    :: Constant.Constant      -> Value 'Constant a

mutable :: Value 'Constant a -> Value 'Mutable a
mutable = ValueMutable

constant :: Value 'Constant a -> Value 'Constant a
constant = id

class Weaken (const :: Constness) where
  weaken :: Value const a -> Value 'Mutable a

instance Weaken 'Constant where
  weaken = mutable

instance Weaken 'Mutable where
  weaken = id

class InjectConstant (const :: Constness) where
  injectConstant :: Constant.Constant -> Value const a

instance InjectConstant 'Mutable where
  injectConstant = ValueMutable . injectConstant

instance InjectConstant 'Constant where
  injectConstant = ValueConstant

data Classification
  = IntegerClass
  | FloatingPointClass
  | PointerClass
  | VectorClass
  | StructureClass
  | LabelClass
  | MetadataClass

class ValueJoin (const :: Constness) where
  vjoin :: Value const a -> BasicBlock (Value const a)

instance ValueJoin 'Mutable where
  vjoin (ValueOperand a) = a >>= return . ValueOperand . return
  vjoin a = return a

instance ValueJoin 'Constant where
  vjoin a = return a

evalConstantBasicBlock
  :: BasicBlock (Value 'Constant a)
  -> Value 'Constant a
evalConstantBasicBlock (BasicBlock v) =
  let m = evalRWST v () (BasicBlockState (error "name") Nothing)
  in fst $ evalState (runFunctionDefinition m) (FunctionDefinitionState [] 0)

asOp
  :: Value const a
  -> BasicBlock AST.Operand
asOp (ValueConstant x) = return $ AST.ConstantOperand x
asOp (ValueMutable x) = asOp x
asOp (ValueOperand x) = x
