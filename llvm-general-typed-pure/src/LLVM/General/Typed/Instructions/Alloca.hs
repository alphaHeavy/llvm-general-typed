{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module LLVM.General.Typed.Instructions.Alloca
  ( alloca
  ) where

import Control.Applicative
import Data.Proxy
import Foreign.Ptr (Ptr)
import GHC.TypeLits
import qualified LLVM.General.AST as AST
import qualified LLVM.General.AST.Constant as Constant

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.FreshName
import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf

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
  ValueOperand . return <$> nameInstruction ty inst
