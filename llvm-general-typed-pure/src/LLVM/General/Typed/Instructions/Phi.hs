{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.Instructions.Phi where

import Control.Applicative
import Data.Proxy
import Data.Traversable
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.AnyValue
import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf

class Phi (f :: * -> *) where
  phi :: ValueOf (Value 'Mutable a) => [(f a, SomeLabel)] -> BasicBlock (Value 'Mutable a)

instance Phi (Value const) where
  phi :: forall a . ValueOf (Value 'Mutable a) => [(Value const a, SomeLabel)] -> BasicBlock (Value 'Mutable a)
  phi incomingValues = do
    -- @TODO: make sure we have evaluated all of the values in the list...
    incomingValues' <- for incomingValues $ \ (val, SomeLabel (Label origin)) -> do
      valOp <- asOp val
      return (valOp, origin)

    let ty = valueType (Proxy :: Proxy (Value 'Mutable a))
    ValueOperand . return <$> nameInstruction ty (AST.Phi ty incomingValues' [])

instance Phi AnyValue where
  phi :: forall a . ValueOf (Value 'Mutable a) => [(AnyValue a, SomeLabel)] -> BasicBlock (Value 'Mutable a)
  phi incomingValues = do
    -- @TODO: make sure we have evaluated all of the values in the list...
    incomingValues' <- for incomingValues $ \ (AnyValue val, SomeLabel (Label origin)) -> do
      valOp <- asOp val
      return (valOp, origin)

    let ty = valueType (Proxy :: Proxy (Value 'Mutable a))
    ValueOperand . return <$> nameInstruction ty (AST.Phi ty incomingValues' [])
