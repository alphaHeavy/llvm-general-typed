{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.Instructions.Apply
  ( Apply(..)
  , ArgumentList
  ) where

import Control.Applicative
import Data.Proxy
import Data.Void
import GHC.Generics
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.ArgumentList
import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.Value

class Apply (args :: [*]) (f :: * -> *) where
  type ApplicationResult args f :: *
  apply :: proxy args -> f p -> BasicBlock [AST.Operand]

instance Apply xs f => Apply xs (M1 i c f) where
  type ApplicationResult xs (M1 i c f) = ApplicationResult xs f
  apply xs = apply xs . unM1

instance (Apply '[a] x, Apply as y) => Apply (a ': as) (x :*: y) where
  type ApplicationResult (a ': as) (x :*: y) = ApplicationResult as y -- The rightmost type is the result
  apply _ (x :*: y) = (++) <$> apply (Proxy :: Proxy '[a]) x <*> apply (Proxy :: Proxy as) y

instance x ~ a => Apply '[x] (K1 i (Value const a)) where
  type ApplicationResult '[x] (K1 i (Value const a)) = a
  apply _ = fmap (:[]) . asOp . unK1

instance f ~ Proxy "Extra arguments applied to function" => Apply '[] (K1 i f) where
  type ApplicationResult '[] (K1 i f) = Void
  apply _ _ = error "Extra arguments applied to function"

instance f ~ Proxy "Insufficient arguments applied to function" => Apply (a ': b ': c) (K1 i f) where
  type ApplicationResult (a ': b ': c) (K1 i f) = Void
  apply _ _ = error "Insufficient arguments applied to function"
