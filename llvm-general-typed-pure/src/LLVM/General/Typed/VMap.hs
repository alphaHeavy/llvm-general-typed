{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.VMap where

import Control.Monad

import LLVM.General.AST (Operand)
import LLVM.General.AST.Constant (Constant)

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueJoin

vmap1
  :: (Constant -> Constant)
  -> (Operand -> BasicBlock Operand)
  -> Value const a
  -> Value const b
vmap1 f _ (ValueConstant x) = ValueConstant (f x)
vmap1 f g (ValueWeakened x) = weaken (vmap1 f g x)
vmap1 _ g x@ValueOperand{}  = ValueOperand (join (g <$> asOp x))
vmap1 _ g (ValuePure x)     = ValueOperand (g x)

vmap1'
  :: (Constant -> Constant)
  -> (Operand -> BasicBlock Operand)
  -> Value const a
  -> BasicBlock (Value const b)
vmap1' f g a = vjoin (vmap1 f g a)

vmap2
  :: forall a b cx cy r .
     (Constant -> Constant -> Constant)
  -> (Operand -> Operand -> BasicBlock Operand)
  -> Value cx a
  -> Value cy b
  -> Value (cx `Weakest` cy) r
vmap2 f g = k where
  j :: Value cx a -> Value cy b -> Value 'Operand r
  j x y = ValueOperand (join (g <$> asOp x <*> asOp y))
  k (ValueConstant x) (ValueConstant y) = ValueConstant (f x y)
  k (ValueWeakened x) (ValueWeakened y) = weaken (vmap2 f g x y)
  k (ValuePure x)     (ValuePure y)     = ValueOperand (g x y)
  -- prepare to experience many pleasures of the GADT... the
  -- coverage condition checker as of GHC 7.10.2 does't know that
  -- 'k x y = j x y' is sufficient to cover the remaining cases so
  -- they need to be enumerated explicitly
  k x@ValueOperand{} y  = j x y
  k x y@ValueOperand{}  = j x y
  k x@ValueWeakened{} y = j x y
  k x y@ValueWeakened{} = j x y
  k x@ValuePure{} y     = j x y
  k x y@ValuePure{}     = j x y

vmap2'
  :: (Constant -> Constant -> Constant)
  -> (Operand -> Operand -> BasicBlock Operand)
  -> Value cx a
  -> Value cy b
  -> BasicBlock (Value (cx `Weakest` cy) r)
vmap2' f g a b = vjoin (vmap2 f g a b)

vmap3
  :: forall a b c cx cy cz r .
     (Constant -> Constant -> Constant -> Constant)
  -> (Operand -> Operand -> Operand -> BasicBlock Operand)
  -> Value cx a
  -> Value cy b
  -> Value cz c
  -> Value (cx `Weakest` cy `Weakest` cz) r
vmap3 f g = k where
  j :: Value cx a -> Value cy b -> Value cz c -> Value 'Operand r
  j x y z = ValueOperand (join (g <$> asOp x <*> asOp y <*> asOp z))
  k (ValueConstant x) (ValueConstant y) (ValueConstant z) = ValueConstant (f x y z)
  k (ValueWeakened x) (ValueWeakened y) (ValueWeakened z) = weaken (vmap3 f g x y z)
  k (ValuePure x)     (ValuePure y)     (ValuePure z)     = ValueOperand (g x y z)
  -- prove we're dealing with a mutable result type, as above
  k x@ValueOperand{} y z  = j x y z
  k x y@ValueOperand{} z  = j x y z
  k x y z@ValueOperand{}  = j x y z
  k x@ValueWeakened{} y z = j x y z
  k x y@ValueWeakened{} z = j x y z
  k x y z@ValueWeakened{} = j x y z
  k x@ValuePure{} y z     = j x y z
  k x y@ValuePure{} z     = j x y z
  k x y z@ValuePure{}     = j x y z

vmap3'
  :: (Constant -> Constant -> Constant -> Constant)
  -> (Operand -> Operand -> Operand -> BasicBlock Operand)
  -> Value cx a
  -> Value cy b
  -> Value cz c
  -> BasicBlock (Value (cx `Weakest` cy `Weakest` cz) r)
vmap3' f g a b c = vjoin (vmap3 f g a b c)
