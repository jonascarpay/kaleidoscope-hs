{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Codegen where

import AST
import Control.Exception
import Control.Monad
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import LLVM.Bindings

gen :: ContextRef -> ModuleRef -> BuilderRef -> Map Ident ValueRef -> Expr -> IO ValueRef
gen _ _ _ bnd (Var name) = case M.lookup name bnd of
  Nothing -> error "unknown var"
  Just var -> pure var
gen ctx _ _ _ (Num x) = do
  typ <- typeDouble ctx
  constReal typ x
gen ctx mdl bld bnd (Bin op l r) = do
  vl <- gen ctx mdl bld bnd l
  vr <- gen ctx mdl bld bnd r
  case op of
    Add -> buildFAdd bld vl vr "addtmp"
    Sub -> buildFSub bld vl vr "subtmp"
    Mul -> buildFMul bld vl vr "multmp"
    Lt -> do
      ui <- buildFCmp bld RealULT vl vr "cmptmp"
      typ <- typeDouble ctx
      buildUIToFP bld ui typ "booltmp"
    _ -> error "no div"
gen ctx mdl bld bnd (Call ident args) = do
  -- TODO for now, we use the LLVM symbol table to handle identifiers
  val <- functionLookup mdl (T.unpack ident) >>= maybe (error "undefined fun ref") pure
  nExpected <- functionCountParams val
  -- TODO for now, we regenerate a double tyval
  typ <- typeDouble ctx
  args' <- traverse (gen ctx mdl bld bnd) args
  let nProvided = length args
  unless (nExpected == nProvided) $ error "argument length mismatch"
  buildCall bld typ val args' nProvided "calltmp"

-- genFunction :: ContextRef -> BuilderRef -> Def -> IO ()

withContext :: (ContextRef -> IO a) -> IO a
withContext = bracket contextCreate contextDispose

withBuilder :: ContextRef -> (BuilderRef -> IO a) -> IO a
withBuilder ctx = bracket (builderCreate ctx) builderDispose

withModule :: String -> ContextRef -> (ModuleRef -> IO a) -> IO a
withModule name ctx = bracket (moduleCreate name ctx) moduleDispose

main :: IO ()
main = do
  _ <- withContext $ \ctx ->
    withModule "module" ctx $ \mdl ->
      withBuilder ctx $ \bld ->
        gen ctx mdl bld mempty (Call "soup" [])
  pure ()
