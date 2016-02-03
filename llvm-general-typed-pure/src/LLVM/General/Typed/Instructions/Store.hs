module LLVM.General.Typed.Instructions.Store
  ( store
  ) where

import Control.Monad.RWS.Lazy
import Foreign.Ptr (Ptr)
import qualified LLVM.General.AST as AST

import LLVM.General.Typed.BasicBlock
import LLVM.General.Typed.Value

store
  :: Bool -- ^ Is this a volatile store?
  -> Value cx (Ptr a) -- ^ Destination address
  -> Value cy a -- ^ Source value
  -> BasicBlock ()
store volatile address value = do
  address' <- asOp address
  value' <- asOp value
  let instr = AST.Store volatile address' value' Nothing 0 []
  tell [AST.Do instr]
