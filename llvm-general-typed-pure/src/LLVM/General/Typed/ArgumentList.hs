{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module LLVM.General.Typed.ArgumentList where

import GHC.TypeLits

-- |
-- Convert a function type @ a -> b -> c -> ... @ to a type level list @ [a, b, c, ...] @
-- to support instance matching without a sentinel or enabling IncoherentInstances.
type family ArgumentList (args :: *) :: [*] where
  ArgumentList (a -> b) = a ': ArgumentList b
  ArgumentList a = '[a]

-- |
-- Return the type at a particular index for a function type.
type family ParameterType (xs :: *) (n :: Nat) :: * where
  ParameterType (a -> b) 0 = a
  ParameterType (a -> b) n = ParameterType b (n - 1)

type family ReturnType (ty :: *) :: * where
  ReturnType (a -> b) = ReturnType b
  ReturnType b = b
