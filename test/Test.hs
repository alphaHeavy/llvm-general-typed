{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Data.Int
import Data.Proxy
import Data.Type.Equality
import Foreign.Ptr (Ptr)

import Instructions

getElementPtrTypeEquality :: ()
getElementPtrTypeEquality = () where
  _ = Refl :: GetElementPtrType (Ptr Int32) (Proxy '[0]) :~: Int32
  _ = Refl :: GetElementPtrType (Ptr Int32) (Proxy '[1]) :~: Int32
  _ = Refl :: GetElementPtrType (Ptr Int32) (Proxy '[100]) :~: Int32
  _ = Refl :: GetElementPtrType (Struct '[Int32]) (Proxy '[0]) :~: Int32
  _ = Refl :: GetElementPtrType (Struct '[Int32, Int64]) (Proxy '[2]) :~: InvalidGetElementPtrIndexBoundsStruct
  _ = Refl :: GetElementPtrType (Struct '[Int32, Ptr Int64]) (Proxy '[1, 0]) :~: Int64
  _ = Refl :: GetElementPtrType (Struct '[Int32, Ptr Int64]) (Proxy '[1, 1, 0]) :~: InvalidGetElementPtrIndexBoundsPtr
  _ = Refl :: GetElementPtrType (Struct '[Struct '[Float, Double], Int64]) (Proxy '[0, 0]) :~: Float
  _ = Refl :: GetElementPtrType (Struct '[Struct '[Float, Double], Int64]) (Proxy '[0, 1]) :~: Double
  _ = Refl :: GetElementPtrType (Struct '[Struct '[Float, Double], Int64]) (Proxy '[1]) :~: Int64
  _ = Refl :: GetElementPtrType (Struct '[Struct '[Float, Double], Ptr Int64]) (Proxy '[1, 0]) :~: Int64

main :: IO ()
main = return ()
