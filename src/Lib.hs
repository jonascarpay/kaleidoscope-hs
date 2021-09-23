module Lib
  ( someFunc,
  )
where

import Foreign
import Foreign.C
import LLVM
import LLVM.Context

foreign import ccall "stdio.h printf" myprintf :: CString -> IO ()

foreign import ccall "math.h sin" c_sin :: CDouble -> IO CDouble

foreign import ccall "math.h sin" c_sin_pure :: CDouble -> CDouble

foreign import ccall "adsfasfdasdfasdf sin" c_sin_doesnt_care_about_libraries :: CDouble -> CDouble

someFunc :: IO ()
someFunc = do
  withCString "hello\n" myprintf
  c_sin 2 >>= print
  withLLVM $ pure ()
  print $ c_sin_pure 2
