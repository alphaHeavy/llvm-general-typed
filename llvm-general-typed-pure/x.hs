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
import LLVM.General.Typed.Num ()
import LLVM.General.Typed.Value

foo :: Module ()
foo = do
  let val :: Value 'Constant Int8
      val = 42 + 9

  namedModule "foo" $ do
    -- _x :: Function C Int64 <- namedFunction_ "bar" $ mdo
    _y :: Function C () <- namedFunction_ "foo" $ do
      void . basicBlock_ $ ret_

    _x :: Function C (Int32 -> Int64) <- namedFunction_ "bar" $ mdo
      p0 <- getParameter (Proxy :: Proxy 0)

      entryBlock <- basicBlock_ $ do
        br secondBlock

      secondBlock <- namedBasicBlock_ (AST.Name "second") $ do
        p0trunc <- trunc p0
        someLocalPtr <- alloca
        store True someLocalPtr $ (99 :: Value 'Constant Int8) + (-99)
        someLocal <- load True someLocalPtr
        x <- val `add` someLocal
        join $ condBr
          <$> cmp someLocal p0trunc
          <*> basicBlock_ (ret <=< ext $ abs x * someLocal + mutable (val - signum 8))
          <*> basicBlock_ (br entryBlock)

      return ()

    return ()

main :: IO ()
main = do
  putStrLn . showPretty . fst $ evalModule foo
