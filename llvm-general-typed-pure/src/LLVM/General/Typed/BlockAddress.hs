{-# LANGUAGE DeriveDataTypeable #-}

module LLVM.General.Typed.BlockAddress where

import Data.Typeable

-- |
-- Opaque type used for 'LLVM.General.Typed.blockAddress' and 'LLVM.General.Typed.indirectBr'
data BlockAddress deriving Typeable
