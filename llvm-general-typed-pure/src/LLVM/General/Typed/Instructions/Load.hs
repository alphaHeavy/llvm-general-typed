{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.Instructions.Load
  ( load
  ) where

import Control.Applicative
import Data.Proxy
import Foreign.Ptr (Ptr)
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf

load
  :: forall a const
   . ValueOf (Value 'Mutable a)
  => Bool
  -> Value const (Ptr a)
  -> BasicBlock (Value 'Mutable a)
load volatile value = do
  value' <- asOp value
  let ty = valueType (Proxy :: Proxy (Value 'Mutable a))
  ValuePure <$> nameInstruction ty (AST.Load volatile value' Nothing 0 [])
