{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.FunctionType where

import Data.Proxy
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf (ValueOf, valueType)

class FunctionType (a :: [*]) where
  functionType :: proxy a -> [AST.Type]

instance (ValueOf (Value 'Operand x), FunctionType xs) => FunctionType (x ': xs) where
  functionType _ = valueType (Proxy :: Proxy (Value 'Operand x)) : functionType (Proxy :: Proxy xs)

instance FunctionType '[] where
  functionType _ = []

-- |
-- Convert a type list from Haskell format (argType1 -> argType2 -> returnType)
-- to ([argType1, argType2], returnType), matching the format expected by llvm
splitFunctionTypes :: [AST.Type] -> Maybe ([AST.Type], AST.Type)
splitFunctionTypes = go [] where
  go  _     [] = Nothing
  go ys    [x] = Just (reverse ys, x)
  go ys (x:xs) = go (x:ys) xs
