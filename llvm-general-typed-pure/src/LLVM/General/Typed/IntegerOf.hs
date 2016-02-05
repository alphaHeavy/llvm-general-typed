{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module LLVM.General.Typed.IntegerOf
  ( IntegerOf(..)
  ) where

import Data.Int
import Data.Proxy
import Data.Word
import GHC.TypeLits

import LLVM.General.Typed.Value
import LLVM.General.Typed.ValueOf

class ValueOf a => IntegerOf (a :: *) where
  type IsSigned a :: Bool
  isSignedInt :: proxy a -> Bool

instance IntegerOf Int8 where
  type IsSigned Int8 = 'True
  isSignedInt _ = True

instance IntegerOf Int16 where
  type IsSigned Int16 = 'True
  isSignedInt _ = True

instance IntegerOf Int32 where
  type IsSigned Int32 = 'True
  isSignedInt _ = True

instance IntegerOf Int64 where
  type IsSigned Int64 = 'True
  isSignedInt _ = True

instance IntegerOf Word8 where
  type IsSigned Word8 = 'False
  isSignedInt _ = False

instance IntegerOf Word16 where
  type IsSigned Word16 = 'False
  isSignedInt _ = False

instance IntegerOf Word32 where
  type IsSigned Word32 = 'False
  isSignedInt _ = False

instance IntegerOf Word64 where
  type IsSigned Word64 = 'False
  isSignedInt _ = False

instance (IntegerOf a, KnownNat n) => IntegerOf (Array n a) where
  type IsSigned (Array n a) = IsSigned a
  isSignedInt _ = isSignedInt (Proxy :: Proxy a)
