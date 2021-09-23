{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module LLVM.Context where

import Control.Monad.Reader
import Foreign

data Context

foreign import ccall "LLVMContextCreate" contextCreate :: IO (Ptr Context)

foreign import ccall "LLVMContextDispose" contextDispose :: Ptr Context -> IO ()

newtype LLVM a = LLVM {unLLVMT :: ReaderT (Ptr Context) IO a}
  deriving (Functor, Monad, Applicative)

withLLVM :: LLVM a -> IO a
withLLVM (LLVM m) = do
  ctx <- contextCreate
  a <- runReaderT m ctx
  contextDispose ctx
  pure a
