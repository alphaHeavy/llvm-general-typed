module AnyValue where

import Value
import ValueOf

data AnyValue (a :: *) where
  AnyValue :: ValueOf (Value const a) => Value const a -> AnyValue a
