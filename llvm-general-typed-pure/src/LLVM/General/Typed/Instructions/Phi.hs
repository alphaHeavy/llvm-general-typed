{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.Instructions.Phi where

import Data.Proxy
import Data.Traversable
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.AnyValue
import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf

class Phi (f :: * -> *) where
  phi :: ValueOf (Value 'Operand a) => [(f a, SomeLabel)] -> BasicBlock (Value 'Operand a)

instance Phi (Value const) where
  phi :: forall a . ValueOf (Value 'Operand a) => [(Value const a, SomeLabel)] -> BasicBlock (Value 'Operand a)
  phi incomingValues = do
    -- @TODO: make sure we have evaluated all of the values in the list...
    incomingValues' <- for incomingValues $ \ (val, SomeLabel (Label origin)) -> do
      valOp <- asOp val
      return (valOp, origin)

    let ty = valueType (Proxy :: Proxy (Value 'Operand a))
    ValuePure <$> nameInstruction ty (AST.Phi ty incomingValues' [])

instance Phi AnyValue where
  phi :: forall a . ValueOf (Value 'Operand a) => [(AnyValue a, SomeLabel)] -> BasicBlock (Value 'Operand a)
  phi incomingValues = do
    -- @TODO: make sure we have evaluated all of the values in the list...
    incomingValues' <- for incomingValues $ \ (AnyValue val, SomeLabel (Label origin)) -> do
      valOp <- asOp val
      return (valOp, origin)

    let ty = valueType (Proxy :: Proxy (Value 'Operand a))
    ValuePure <$> nameInstruction ty (AST.Phi ty incomingValues' [])
