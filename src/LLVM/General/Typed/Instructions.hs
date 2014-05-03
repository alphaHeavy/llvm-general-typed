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
  , InBounds(..)
  , getElementPtr
  , getElementPtr0
  -- ** Conversion Operations
  , trunc
  , ext
  , inttofp
  , fptoint
  -- , ptrtoint
  -- , inttoptr
  , bitcast
  -- , addrspacecast
  -- * Other Operations
  , undef
  -- *** Comparisons
  , icmp
  , fcmp
  , Cmp(..)
  -- *** asdf
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
  :: (ClassificationOf (Value 'Constant a) ~ IntegerClass)
  => Value const a
  -> Label -- default
  -> [(Value 'Constant a, Label)]
  -> BasicBlock (Terminator ())
switch value (Label defaultDest) dests = do
  valueOp <- asOp value
  let dests' = [(val, dest) | (ValueConstant val, Label dest) <- dests]
  setTerminator $ AST.Switch valueOp defaultDest dests' []
  return $ Terminator ()

blockAddress :: Function cconv a -> Label -> BasicBlock (Value 'Constant BlockAddress)
blockAddress = undefined

indirectBr :: Value 'Constant BlockAddress -> [Label] -> BasicBlock (Terminator ())
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

fence :: AST.Atomicity -> BasicBlock ()
fence atomicity = do
  let instr = AST.Fence atomicity []
  tell [AST.Do instr]
