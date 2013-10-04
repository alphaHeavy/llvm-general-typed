{-# LANGUAGE TypeOperators #-}

module VMap where

import Control.Applicative
import Control.Monad

import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import BasicBlock
import Value

vmap1
  :: (Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> BasicBlock AST.Operand)
  -> Value const a
  -> Value const b
vmap1 f _ (ValueConstant x) = ValueConstant (f x)
vmap1 f g (ValueMutable x)  = weaken (vmap1 f g x)
vmap1 _ g x@ValueOperand{}  = ValueOperand (join (g <$> asOp x))

vmap1'
  :: (ValueJoin const)
  => (Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> BasicBlock AST.Operand)
  -> Value const a
  -> BasicBlock (Value const b)
vmap1' f g a = vjoin (vmap1 f g a)

vmap2
  :: forall a b cx cy r .
     (Constant.Constant -> Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> AST.Operand -> BasicBlock AST.Operand)
  -> Value cx a
  -> Value cy b
  -> Value (Weakest cx cy) r
vmap2 f g = k where
  j :: Value cx a -> Value cy b -> Value 'Mutable r
  j x y = ValueOperand (join (g <$> asOp x <*> asOp y))
  k (ValueConstant x) (ValueConstant y) = ValueConstant (f x y)
  k (ValueMutable x)  (ValueMutable y)  = weaken (vmap2 f g x y)
  -- prepare to experience many pleasures of the GADT
  k x@ValueOperand{} y = j x y
  k x y@ValueOperand{} = j x y
  k x@ValueMutable{} y = j x y
  k x y@ValueMutable{} = j x y

vmap2'
  :: (ValueJoin (Weakest cx cy))
  => (Constant.Constant -> Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> AST.Operand -> BasicBlock AST.Operand)
  -> Value cx a
  -> Value cy b
  -> BasicBlock (Value (Weakest cx cy) r)
vmap2' f g a b = vjoin (vmap2 f g a b)

vmap3
  :: forall a b c cx cy cz r .
     (Constant.Constant -> Constant.Constant -> Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> AST.Operand -> AST.Operand -> BasicBlock AST.Operand)
  -> Value cx a
  -> Value cy b
  -> Value cz c
  -> Value (cx `Weakest` cy `Weakest` cz) r
vmap3 f g = k where
  j :: Value cx a -> Value cy b -> Value cz c -> Value 'Mutable r
  j x y z = ValueOperand (join (g <$> asOp x <*> asOp y <*> asOp z))
  k (ValueConstant x) (ValueConstant y) (ValueConstant z) = ValueConstant (f x y z)
  k (ValueMutable x)  (ValueMutable y)  (ValueMutable z)  = weaken (vmap3 f g x y z)
  -- prove we're dealing with a mutable result type
  k x@ValueOperand{} y z = j x y z
  k x y@ValueOperand{} z = j x y z
  k x y z@ValueOperand{} = j x y z
  k x@ValueMutable{} y z = j x y z
  k x y@ValueMutable{} z = j x y z
  k x y z@ValueMutable{} = j x y z

vmap3'
  :: (ValueJoin (cx `Weakest` cy `Weakest` cz))
  => (Constant.Constant -> Constant.Constant -> Constant.Constant -> Constant.Constant)
  -> (AST.Operand -> AST.Operand -> AST.Operand -> BasicBlock AST.Operand)
  -> Value cx a
  -> Value cy b
  -> Value cz c
  -> BasicBlock (Value (cx `Weakest` cy `Weakest` cz) r)
vmap3' f g a b c = vjoin (vmap3 f g a b c)

