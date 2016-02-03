{-# LANGUAGE CPP #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

#include "MachDeps.h"

module LLVM.General.Typed.Instructions.Alloca
  ( alloca
  ) where

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
  :: forall a
   . ValueOf a -- Inspect the runtime type
  => KnownNat (ElementsOf a) -- Inspect the number of elements
  => BasicBlock (Value 'Operand (Ptr a)) -- ^ An instance allocated on the stack
alloca = do
  let ty = valueType (Proxy :: Proxy a)
      ne = natVal (Proxy :: Proxy (ElementsOf a))
      inst = AST.Alloca ty (Just (AST.ConstantOperand (Constant.Int WORD_SIZE_IN_BITS ne))) 0 []
  ValuePure <$> nameInstruction ty inst
