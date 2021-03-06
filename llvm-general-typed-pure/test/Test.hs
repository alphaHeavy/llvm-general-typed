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

import LLVM.General.Typed
import LLVM.General.Typed.CallingConv
import LLVM.General.Typed.DefineBasicBlock
import LLVM.General.Typed.Instructions
import LLVM.General.Typed.Module
import LLVM.General.Typed.Value

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

  _ = Refl :: GetElementPtrType (Array 100 (Array 100 Int64)) (Index (Value 'Operand Int32, Value 'Operand Int32)) :~: Int64

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
          _x :: Function C Int32 <- namedFunction_ "bar" $
            void . basicBlock_ $ do
              structPtr :: Value 'Operand (Ptr (Struct '[Int32, Ptr Int32])) <- alloca
              memberPtr :: Value 'Operand (Ptr Int32) <- getElementPtr InBounds structPtr (Proxy :: Proxy '[0, 1, 0])
              val <- load True memberPtr
              ret val

          return ()

  putStrLn $ showPretty ast
