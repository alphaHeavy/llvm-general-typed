{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}

module Function where

import Foreign.Ptr (Ptr)

import CallingConv
import Value

-- |
-- 'Function's are 'Constant' 'Value's with a specific 'CallingConv'
newtype Function (cconv :: CallingConv) (a :: *) = Function{functionValue :: Value 'Constant (Ptr a)}
