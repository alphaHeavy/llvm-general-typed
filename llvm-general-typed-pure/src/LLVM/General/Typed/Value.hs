{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Value where

import Control.Monad.RWS.Lazy
import Control.Monad.State.Lazy
import Data.Typeable
import GHC.TypeLits (Nat)

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FunctionDefinition

data Constness = Constant | Mutable

type family Weakest (x :: k) (y :: k) :: k where
  Weakest 'Constant 'Constant = 'Constant
  Weakest x         y         = 'Mutable

data Value (const :: Constness) (a :: *) where
  ValueMutable  :: Value 'Constant a      -> Value 'Mutable a
  ValueOperand  :: BasicBlock AST.Operand -> Value 'Mutable a
  ValueConstant :: Constant.Constant      -> Value 'Constant a

data Struct (xs :: [*]) = Struct deriving Typeable
data Array (n :: Nat) (a :: *) = Array deriving Typeable

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

evalConstantBasicBlock
  :: BasicBlock (Value 'Constant a)
  -> Value 'Constant a
evalConstantBasicBlock (BasicBlock v) =
  -- since this is only being used for Num instances we want to catch accidental
  -- evaluation of a block that contains mixed constant and operand values
  let m = evalRWST v () (BasicBlockState (error "evalConstantBasicBlock discards named instructions, is this what you want?") Nothing)
  in fst $ evalState (runFunctionDefinition m) (FunctionDefinitionState [] 0 [])

asOp
  :: Value const a
  -> BasicBlock AST.Operand
asOp (ValueConstant x) = return $ AST.ConstantOperand x
asOp (ValueMutable x) = asOp x
asOp (ValueOperand x) = x