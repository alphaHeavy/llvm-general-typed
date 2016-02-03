{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.Instructions.Load
  ( load
  ) where

import Data.Proxy
import Foreign.Ptr (Ptr)
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf

load
  :: forall a const
   . ValueOf a
  => Bool -- ^ Is this a volatile load?
  -> Value const (Ptr a) -- ^ Source address
  -> BasicBlock (Value 'Operand a) -- ^ Dereferenced 'Value'
load volatile value = do
  value' <- asOp value
  let ty = valueType (Proxy :: Proxy a)
  ValuePure <$> nameInstruction ty (AST.Load volatile value' Nothing 0 [])
