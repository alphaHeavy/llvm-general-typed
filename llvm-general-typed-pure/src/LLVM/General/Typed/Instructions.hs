{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions
  ( -- * Instructions
    -- ** Terminator Instructions
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
  -- ** Binary Operations
  , add
  , sub
  , mul
  , LLVM.General.Typed.Instructions.Div.div
  , LLVM.General.Typed.Instructions.Rem.rem
  -- ** Specializations
  , Add
  , Sub
  , Mul
  , Div
  , Rem
  -- ** Bitwise Binary Operation
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
  -- ** Memory Access and Addressing Operations
  , alloca
  , load
  , store
  , fence
  -- , cmpxchg
  -- , atomicrmw
  -- *** GetElementPtr
  , Bounds(..)
  , getElementPtr
  , getElementPtr0
  , tryGetElementPtr
  , GetElementIndex
  , GetElementPtrConstness
  , GetElementPtrType
  , Index
  , InvalidGetElementPtrIndexBoundsStruct
  -- ** Conversion Operations
  , trunc
  , ext
  , inttofp
  , fptoint
  -- , ptrtoint
  -- , inttoptr
  , bitcast
  -- ** Specializations
  , Trunc
  , Ext
  -- , addrspacecast
  -- * Other Operations
  , undef
  -- *** Comparisons
  , icmp
  , fcmp
  , Cmp(..)
  -- *** Value selection
  , Phi(..)
  , select
  -- *** Function invocation
  , call
  , tailcall
  -- , va_arg
  -- , landingpad
  ) where

import Control.Monad.RWS.Lazy
import Data.Proxy
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant
import qualified LLVM.General.AST.FloatingPointPredicate as FloatingPointPredicate
import qualified LLVM.General.AST.IntegerPredicate as IntegerPredicate

import LLVM.General.Typed.ArgumentList
import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.BlockAddress
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Function
import LLVM.General.Typed.Instructions.Add
import LLVM.General.Typed.Instructions.Alloca
import LLVM.General.Typed.Instructions.Bitcast
import LLVM.General.Typed.Instructions.Call
import LLVM.General.Typed.Instructions.Div
import LLVM.General.Typed.Instructions.Extend
import LLVM.General.Typed.Instructions.FPToInt
import LLVM.General.Typed.Instructions.GetElementPtr
import LLVM.General.Typed.Instructions.IntToFP
import LLVM.General.Typed.Instructions.Invoke
import LLVM.General.Typed.Instructions.Load
import LLVM.General.Typed.Instructions.Mul
import LLVM.General.Typed.Instructions.Phi
import LLVM.General.Typed.Instructions.Rem
import LLVM.General.Typed.Instructions.Ret
import LLVM.General.Typed.Instructions.Select
import LLVM.General.Typed.Instructions.Store
import LLVM.General.Typed.Instructions.Sub
import LLVM.General.Typed.Instructions.Trunc
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf
import LLVM.General.Typed.VMap

-- |
-- Conditionally branch to a target.
condBr
  :: Value const Bool -- ^ Conditional
  -> Label rty -- ^ Branch target if the conditional is 'True'
  -> Label rty -- ^ Branch target if the conditional is 'False'
  -> BasicBlock (Terminator rty ())
condBr condition (Label trueDest) (Label falseDest) = do
  conditionOp <- asOp condition
  setTerminator $ AST.CondBr conditionOp trueDest falseDest []
  return $ Terminator ()

-- |
-- Unconditionally branch to a target.
br :: Label rty -> BasicBlock (Terminator rty ())
br (Label dest) = do
  setTerminator $ AST.Br dest []
  return $ Terminator ()

-- |
switch
  :: (ClassificationOf a ~ 'IntegerClass)
  => Value const a -- ^ Branch index
  -> Label rty -- ^ Default branch target
  -> [(Value 'Constant a, Label rty)] -- ^ List of index and branch target pairs
  -> BasicBlock (Terminator rty ())
switch value (Label defaultDest) dests = do
  valueOp <- asOp value
  let dests' = [(val, dest) | (ValueConstant val, Label dest) <- dests]
  setTerminator $ AST.Switch valueOp defaultDest dests' []
  return $ Terminator ()

blockAddress :: Function cconv ty -> Label (ReturnType ty) -> BasicBlock (Value 'Constant BlockAddress)
blockAddress = undefined

indirectBr :: Value 'Constant BlockAddress -> [Label rty] -> BasicBlock (Terminator rty ())
indirectBr = undefined

resume :: Value const a -> BasicBlock (Terminator rty ())
resume = undefined

unreachable
  :: BasicBlock (Terminator rty ())
unreachable = do
  setTerminator $ AST.Unreachable []
  return $ Terminator ()

-- |
-- An undefined value of any type.
undef
  :: forall a
   . ValueOf a
  => BasicBlock (Value 'Constant a) -- ^ Result
undef = do
  let val = Constant.Undef $ valueType (Proxy :: Proxy a)
  return $ ValueConstant val

-- |
-- Compare two integers according to a predicate.
icmp
  :: ClassificationOf a ~ 'IntegerClass
  => IntegerPredicate.IntegerPredicate
  -> Value const'a a -- ^ First operand
  -> Value const'b a -- ^ Second operand
  -> BasicBlock (Value (const'a `Weakest` const'b) Bool) -- ^ Result
icmp p = vmap2' f g where
  f = Constant.ICmp p
  ty = valueType (Proxy :: Proxy Bool)
  g x y = nameInstruction ty $ AST.ICmp p x y []

-- |
-- Compare two floats according to a predicate.
fcmp
  :: ClassificationOf a ~ 'FloatingPointClass
  => FloatingPointPredicate.FloatingPointPredicate
  -> Value const'a a -- ^ First operand
  -> Value const'b a -- ^ Second operand
  -> BasicBlock (Value (const'a `Weakest` const'b) Bool) -- ^ Result
fcmp p = vmap2' f g where
  f = Constant.FCmp p
  ty = valueType (Proxy :: Proxy Bool)
  g x y = nameInstruction ty $ AST.FCmp p x y []

-- |
-- An incomplete type agnostic comparator.
-- Currently only tests for value equality.
class Cmp (classification :: Classification) where
  cmp
    :: (ClassificationOf a ~ classification)
    => Value const'a a -- ^ First operand
    -> Value const'b a -- ^ Second operand
    -> BasicBlock (Value (const'a `Weakest` const'b) Bool) -- ^ Result

instance Cmp 'IntegerClass where
  cmp = vmap2' f g where
    f = Constant.ICmp IntegerPredicate.EQ
    ty = valueType (Proxy :: Proxy Bool)
    g x y = nameInstruction ty $ AST.ICmp IntegerPredicate.EQ x y []

instance Cmp 'FloatingPointClass where
  cmp = vmap2' f g where
    f = Constant.FCmp FloatingPointPredicate.OEQ
    ty = valueType (Proxy :: Proxy Bool)
    g x y = nameInstruction ty $ AST.FCmp FloatingPointPredicate.OEQ x y []

fence
  :: AST.Atomicity -- ^ Atomicity ordering constraint
  -> BasicBlock ()
fence atomicity = do
  let instr = AST.Fence atomicity []
  tell [AST.Do instr]
