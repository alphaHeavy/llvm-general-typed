{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module LLVM.General.Typed.ArgumentList
  ( ArgumentList
  ) where

-- |
-- Convert a function type @ a -> b -> c -> ... @ to a type level list @ [a, b, c, ...] @
-- to support instance matching without a sentinel or enabling IncoherentInstances
type family ArgumentList (args :: *) :: [*] where
  ArgumentList (a -> b) = a ': ArgumentList b
  ArgumentList a = '[a]
