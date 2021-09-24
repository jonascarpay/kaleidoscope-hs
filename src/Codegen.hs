module Codegen where

import AST
import Control.Exception
import Foreign.C
import LLVM.Bindings

gen :: ContextRef -> BuilderRef -> Expr -> IO ValueRef
gen ctx _ (Num x) = do
  typ <- typeDouble ctx
  constReal typ x
gen ctx bld (Bin op l r) = do
  vl <- gen ctx bld l
  vr <- gen ctx bld r
  case op of
    Add -> buildFAdd bld vl vr "addtmp"
    Sub -> buildFSub bld vl vr "subtmp"
    Mul -> buildFMul bld vl vr "multmp"
    Lt -> do
      ui <- buildFCmp bld RealULT vl vr "cmptmp"
      typ <- typeDouble ctx
      buildUIToFP bld ui typ "booltmp"
    _ -> error "no div"
gen _ _ _ = undefined

withContext :: (ContextRef -> IO a) -> IO a
withContext = bracket contextCreate contextDispose

withBuilder :: ContextRef -> (BuilderRef -> IO a) -> IO a
withBuilder ctx = bracket (builderCreate ctx) builderDispose

main :: IO ()
main = do
  withContext $ \ctx ->
    withBuilder ctx $ \bld ->
      gen ctx bld (Num 123)
  pure ()
