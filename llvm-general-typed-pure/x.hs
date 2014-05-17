{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad
import Data.Int
import Data.Proxy
import qualified LLVM.General.AST as AST
import LLVM.General.PrettyPrint (showPretty)

import LLVM.General.Typed
import LLVM.General.Typed.CallingConv
import LLVM.General.Typed.DefineBasicBlock
import LLVM.General.Typed.Module
import LLVM.General.Typed.Instructions
import LLVM.General.Typed.Num ()
import LLVM.General.Typed.Value

foo :: Module ()
foo = do
  let val :: Value 'Constant Int8
      val = 42 + 9

  namedModule "foo" $ do
    _x :: Function C (Proxy [Int, Int]) <- namedFunction_ "bar" $ mdo
      entryBlock <- basicBlock_ $ do
        br secondBlock

      secondBlock <- namedBasicBlock_ (AST.Name "second") $ do
        someLocalPtr <- alloca
        store True someLocalPtr (99 :: Value 'Constant Int8)
        someLocal <- load True someLocalPtr
        x <- val `add` someLocal
        join $ condBr
          <$> cmp someLocal (mutable 99)
          <*> basicBlock_ (ret $ abs x * someLocal + mutable (val - signum 8))
          <*> basicBlock_ (br entryBlock)

      return ()

    return ()

main :: IO ()
main = do
  putStrLn . showPretty . fst $ evalModule foo
