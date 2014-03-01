{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}

module LLVM.General.Typed.AnyValue where

import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf

data AnyValue (a :: *) where
  AnyValue :: ValueOf (Value const a) => Value const a -> AnyValue a
