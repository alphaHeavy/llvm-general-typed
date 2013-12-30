{-# LANGUAGE DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

import Control.Monad
import Data.Int
import Data.Proxy
import Data.Type.Equality
import Foreign.Ptr (Ptr)
import GHC.Generics (Rep)

import LLVM.General.PrettyPrint (showPretty)
import Test.Tasty
import Test.Tasty.QuickCheck as QC
import Test.Tasty.HUnit

import DefineBasicBlock
import Instructions
import Module
import Value

getElementPtrTypeEquality :: ()
getElementPtrTypeEquality = () where
  _ = Refl :: GetElementPtrType (Ptr Int32) (Proxy '[0]) :~: Int32
  _ = Refl :: GetElementPtrType (Ptr Int32) (Proxy '[1]) :~: Int32
  _ = Refl :: GetElementPtrType (Ptr Int32) (Proxy '[100]) :~: Int32
  _ = Refl :: GetElementPtrType (Struct '[Int32]) (Proxy '[0]) :~: Int32
  _ = Refl :: GetElementPtrType (Struct '[Int32, Int64]) (Proxy '[2]) :~: InvalidGetElementPtrIndexBoundsStruct
  _ = Refl :: GetElementPtrType (Struct '[Int32, Ptr Int64]) (Proxy '[1, 0]) :~: Int64
  -- _ = Refl :: GetElementPtrType (Struct '[Int32, Ptr Int64]) (Proxy '[1, 1, 0]) :~: InvalidGetElementPtrIndexBoundsPtr
  _ = Refl :: GetElementPtrType (Struct '[Struct '[Float, Double], Int64]) (Proxy '[0, 0]) :~: Float
  _ = Refl :: GetElementPtrType (Struct '[Struct '[Float, Double], Int64]) (Proxy '[0, 1]) :~: Double
  _ = Refl :: GetElementPtrType (Struct '[Struct '[Float, Double], Int64]) (Proxy '[1]) :~: Int64
  _ = Refl :: GetElementPtrType (Struct '[Struct '[Float, Double], Ptr Int64]) (Proxy '[1, 0]) :~: Int64

  _ = Refl :: GetElementPtrType (Array 100 Int64) (Proxy '[0]) :~: Int64
  _ = Refl :: GetElementPtrType (Array 100 Int64) (Proxy '[100]) :~: Int64
  _ = Refl :: GetElementPtrType (Array 100 Int64) (Proxy '[112]) :~: Int64

  _ = Refl :: GGetElementPtrType (Array 100 (Array 100 Int64)) (Rep (Value 'Mutable Int32, Value 'Mutable Int32)) :~: Int64

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [] -- scProps, qcProps]

unitTests :: TestTree
unitTests = testGroup "Unit tests"
  [ testCase "getElementPtrTests" $ getElementPtrTests
  ]

getElementPtrTests :: Assertion
getElementPtrTests = do
  let ast = fst . evalModule $ do
        namedModule "foo" $ do
          void . namedFunction "bar" $
            basicBlock $ do
              structPtr :: Value 'Mutable (Ptr (Struct '[Int32, Ptr Int32])) <- alloca
              memberPtr :: Value 'Mutable (Ptr Int32) <- getElementPtr InBounds structPtr (Proxy :: Proxy '[0, 1, 0])
              val <- load memberPtr
              ret val

  putStrLn $ showPretty ast
