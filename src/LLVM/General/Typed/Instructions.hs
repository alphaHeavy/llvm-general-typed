{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions
  ( -- * Terminator Instructions
    ret
  , ret_
  , condBr
  , br
  , switch
  , blockAddress
  , indirectBr
  , invoke
  , unreachable
  , resume
  -- * Binary Operations
  , add
  -- , sub
  -- , mul
  -- , div
  -- , rem
  -- * Bitwise Binary Operation
  -- , shl
  -- , lshr
  -- , ashr
  -- , and
  -- , or
  -- , xor
  -- * Vector Operations
  -- , extractelement
  -- , insertelement
  -- , shufflevector
  -- * Aggregate Operations
  -- , extractvalue
  -- , insertvalue
  -- * Memory Access and Addressing Operations
  , alloca
  , load
  , store
  -- , fence
  -- , cmpxchg
  -- , atomicrmw
  -- ** GetElementPtr
  , InBounds(..)
  , getElementPtr
  , getElementPtr0
  -- * Conversion Operations
  , trunc
  -- , zext
  -- , sext
  -- , fptoui
  -- , fptosi
  -- , uitofp
  -- , sitofp
  -- , ptrtoint
  -- , inttoptr
  , bitcast
  -- , addrspacecast
  -- * Other Operations
  , undef
  -- ** Comparisons
  , icmp
  , fcmp
  , Cmp(..)
  -- ** asdf
  , Phi(..)
  , select
  -- ** Function invocation
  , call
  -- , va_arg
  -- , landingpad
  ) where

import Control.Applicative
import Control.Monad.RWS.Lazy
import Data.Proxy
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.FloatingPointPredicate as FloatingPointPredicate
import qualified LLVM.General.AST.IntegerPredicate as IntegerPredicate

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.BlockAddress
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Function
import LLVM.General.Typed.Instructions.Call
import LLVM.General.Typed.Instructions.GetElementPtr
import LLVM.General.Typed.Instructions.Invoke
import LLVM.General.Typed.Instructions.Phi
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueJoin
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.VMap

ret
  :: ValueOf (Value const a)
  => Value const a
  -> BasicBlock (Terminator ())
ret value = do
  -- name the value, emitting instructions as necessary
  valueOp <- asOp value
  setTerminator $ AST.Ret (Just valueOp) []
  -- @TODO: replace with LocalReference ?
  return $ Terminator ()

ret_ :: BasicBlock (Terminator ())
ret_ = do
  setTerminator $ AST.Ret Nothing []
  return $ Terminator ()

condBr
  :: Value const Bool
  -> Label
  -> Label
  -> BasicBlock (Terminator ())
condBr condition (Label trueDest) (Label falseDest) = do
  conditionOp <- asOp condition
  setTerminator $ AST.CondBr conditionOp trueDest falseDest []
  return $ Terminator ()

br :: Label -> BasicBlock (Terminator ())
br (Label dest) = do
  setTerminator $ AST.Br dest []
  return $ Terminator ()

switch
  :: ( ClassificationOf (Value const a)     ~ IntegerClass,
       ClassificationOf (Value 'Constant a) ~ IntegerClass)
  => Value const a
  -> Label -- default
  -> [(Value 'Constant a, Label)]
  -> BasicBlock (Terminator ())
switch value (Label defaultDest) dests = do
  valueOp <- asOp value
  let dests' = [(val, dest) | (ValueConstant val, Label dest) <- dests]
  setTerminator $ AST.Switch valueOp defaultDest dests' []
  return $ Terminator ()

blockAddress :: Function cconv a -> Label -> Ptr BlockAddress
blockAddress = undefined

indirectBr :: Ptr BlockAddress -> [Label] -> BasicBlock (Terminator ())
indirectBr = undefined

resume :: Value const a -> BasicBlock (Terminator ())
resume = undefined

unreachable
  :: BasicBlock (Terminator ())
unreachable = do
  setTerminator $ AST.Unreachable []
  return $ Terminator ()

undef
  :: forall a .
     ValueOf (Value 'Constant a)
  => BasicBlock (Value 'Constant a)
undef = do
  let val = Constant.Undef $ valueType (Proxy :: Proxy (Value 'Constant a))
  return $ ValueConstant val

alloca
  :: forall a .
     ( ValueOf (Value 'Mutable a)
     , KnownNat (ElementsOf (Value 'Mutable a)))
  => BasicBlock (Value 'Mutable (Ptr a))
alloca = do
  let ty = valueType (Proxy :: Proxy (Value 'Mutable a))
      ne = natVal (Proxy :: Proxy (ElementsOf (Value 'Mutable a)))
  -- @TODO: the hardcoded 64 should probably be the target word size?
      inst = AST.Alloca ty (Just (AST.ConstantOperand (Constant.Int 64 ne))) 0 []
  ValueOperand . return <$> nameInstruction inst

load
  :: Value const (Ptr a)
  -> BasicBlock (Value 'Mutable a)
load x = do
  x' <- asOp x
  ValueOperand . return <$> nameInstruction (AST.Load False x' Nothing 0 [])

store
  :: Value cx (Ptr a)
  -> Value cy a
  -> BasicBlock ()
store address value = do
  address' <- asOp address
  value' <- asOp value
  let instr = AST.Store False address' value' Nothing 0 []
  tell [AST.Do instr]

class Name (const :: Constness) where
  name :: Value const a -> BasicBlock (Value const a)

instance Name 'Constant where
  name = return

instance Name 'Mutable where
  name (ValueMutable val) = ValueMutable <$> name val
  name (ValueOperand _val) = do
    _n <- freshName
    undefined

{-
name :: String -> Value const a -> BasicBlock (Value const a)
name = undefined

name_ :: Value const a -> BasicBlock (Value const a)
name_ = undefined
-}

trunc
  :: forall a b const .
     ( ClassificationOf (Value const a) ~ IntegerClass, ClassificationOf (Value const b) ~ IntegerClass
     , ValueOf (Value const b)
     , BitsOf (Value const b) + 1 <= BitsOf (Value const a))
  => Value const a
  -> BasicBlock (Value const b)
trunc = vmap1' f g where
  vt = valueType (Proxy :: Proxy (Value const b))
  f v = Constant.Trunc v vt
  g v = nameInstruction $ AST.Trunc v vt []

bitcast
  :: forall a b const .
     ( BitsOf (Value const a) ~ BitsOf (Value const b)
     , ValueOf (Value const b))
  => Value const a
  -> BasicBlock (Value const b)
bitcast = vmap1' f g where
  vt = valueType (Proxy :: Proxy (Value const b))
  f v = Constant.BitCast v vt
  g v = nameInstruction $ AST.BitCast v vt []

class Add (classification :: Classification) where
  vadd
    :: ClassificationOf (Value (cx `Weakest` cy) a) ~ classification
    => Value cx a
    -> Value cy a
    -> Value (cx `Weakest` cy) a

instance Add 'IntegerClass where
 vadd = vmap2 f g where
   f = Constant.Add False False
   g x y = nameInstruction $ AST.Add False False x y []

instance Add 'FloatingPointClass where
 vadd = vmap2 f g where
   f = Constant.FAdd
   g x y = nameInstruction $ AST.FAdd x y []


add
  :: (Add (ClassificationOf (Value (cx `Weakest` cy) a)))
  => Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) a)
add x y = vjoin $ vadd x y

-- the condition constness must match the result constness. this implies that
-- if both true and false values are constant the switch condition must also be
-- a constant. if you want a constant condition but mutable values (for some reason...)
-- just wrap the condition with 'mutable'
select
  :: Value cc Bool
  -> Value ct a
  -> Value cf a
  -> BasicBlock (Value (cc `Weakest` ct `Weakest` cf) a)
select = vmap3' f g where
  f = Constant.Select
  g c t f' = nameInstruction $ AST.Select c t f' []

icmp
  :: (ClassificationOf (Value (cx `Weakest` cy) a) ~ IntegerClass)
  => IntegerPredicate.IntegerPredicate
  -> Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) Bool)
icmp p = vmap2' f g where
  f = Constant.ICmp p
  g x y = nameInstruction $ AST.ICmp p x y []

fcmp
  :: (ClassificationOf (Value (cx `Weakest` cy) a) ~ FloatingPointClass)
  => FloatingPointPredicate.FloatingPointPredicate
  -> Value cx a
  -> Value cy a
  -> BasicBlock (Value (cx `Weakest` cy) Bool)
fcmp p = vmap2' f g where
  f = Constant.FCmp p
  g x y = nameInstruction $ AST.FCmp p x y []

class Cmp (classification :: Classification) where
  cmp
    :: (ClassificationOf (Value (cx `Weakest` cy) a) ~ classification)
    => Value cx a
    -> Value cy a
    -> BasicBlock (Value (cx `Weakest` cy) Bool)

instance Cmp 'IntegerClass where
  cmp = vmap2' f g where
    f = Constant.ICmp IntegerPredicate.EQ
    g x y = nameInstruction $ AST.ICmp IntegerPredicate.EQ x y []

instance Cmp 'FloatingPointClass where
  cmp = vmap2' f g where
    f = Constant.FCmp FloatingPointPredicate.OEQ
    g x y = nameInstruction $ AST.FCmp FloatingPointPredicate.OEQ x y []
