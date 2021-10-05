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
import System.Exit qualified as Sys

genExpr :: ContextRef -> ModuleRef -> BuilderRef -> Map Ident ValueRef -> Expr -> IO ValueRef
genExpr _ _ _ bnd (Var name) = case M.lookup name bnd of
  Nothing -> error "unknown var"
  Just var -> pure var
genExpr ctx _ _ _ (Num x) = do
  typ <- typeDouble ctx
  constReal typ x
genExpr ctx mdl bld bnd (Bin op l r) = do
  vl <- genExpr ctx mdl bld bnd l
  vr <- genExpr ctx mdl bld bnd r
  case op of
    Add -> buildFAdd bld vl vr "addtmp"
    Sub -> buildFSub bld vl vr "subtmp"
    Mul -> buildFMul bld vl vr "multmp"
    Lt -> do
      ui <- buildFCmp bld RealULT vl vr "cmptmp"
      typ <- typeDouble ctx
      buildUIToFP bld ui typ "booltmp"
    _ -> error "no div"
genExpr ctx mdl bld bnd (Call ident args) = do
  -- TODO for now, we use the LLVM symbol table to handle identifiers
  val <- functionLookup mdl (T.unpack ident) >>= maybe (error "undefined fun ref") pure
  nExpected <- functionCountParams val
  -- TODO for now, we regenerate a double tyval
  typ <- typeDouble ctx
  args' <- traverse (genExpr ctx mdl bld bnd) args
  let nProvided = length args
  unless (nExpected == nProvided) $ error "argument length mismatch"
  buildCall bld typ val args' nProvided "calltmp"

genProto :: ContextRef -> ModuleRef -> FnProto -> IO (ValueRef, [ValueRef])
genProto ctx mdl (FnProto name args) = do
  typ <- typeDouble ctx
  fntyp <- typeFunction typ (typ <$ args) False
  fnVal <- functionAdd mdl (T.unpack name) fntyp
  linkageSet fnVal ExternalLinkage
  args' <- forM (zip args [0 ..]) $ \(arg, ix) -> do
    argVal <- functionGetParam fnVal ix
    valueSetName argVal (T.unpack arg)
    pure argVal
  pure (fnVal, args')

genFunction :: ContextRef -> ModuleRef -> PassManagerRef -> FnDef -> IO ValueRef
genFunction ctx mdl pm (FnDef proto@(FnProto name args) body) = do
  functionLookup mdl (T.unpack name) >>= \mFun -> do
    (fnVal, argVals) <- case mFun of
      Nothing -> genProto ctx mdl proto
      -- Function already declared, now adding a body
      Just fnVal -> do
        nArgs <- functionCountParams fnVal
        -- TODO don't die, just stop evaluation, but clean up
        unless (nArgs == length args) $ Sys.die "invalid length"
        nBlocks <- functionCountBasicBlocks fnVal
        unless (nBlocks == 0) $ Sys.die "function already has body"
        argVals <- forM (take nArgs [0 ..]) $ functionGetParam fnVal
        pure (fnVal, argVals)
    block <- basicBlockAppend ctx fnVal "entry"
    val <- withBuilder ctx $ \bld -> do
      builderSetInsertPoint bld block
      val <- genExpr ctx mdl bld (M.fromList (zip args argVals)) body
      buildRet bld val
    -- TODO
    -- passing `val` instead of `fnVal` here _segfaults_!
    -- An extra layer of segfault-proofing might be nice.
    _ <- functionVerify fnVal PrintMessageAction
    _ <- fpmRun pm fnVal
    pure val

-- TODO Remoove dangling invalid functions

withContext :: (ContextRef -> IO a) -> IO a
withContext = bracket contextCreate contextDispose

withBuilder :: ContextRef -> (BuilderRef -> IO a) -> IO a
withBuilder ctx = bracket (builderCreate ctx) builderDispose

withModule :: String -> ContextRef -> (ModuleRef -> IO a) -> IO a
withModule name ctx = bracket (moduleCreate name ctx) moduleDispose

withFunctionPassManager :: ModuleRef -> (PassManagerRef -> IO a) -> IO a
withFunctionPassManager mdl k = bracket (fpmCreate mdl) fpmDispose $ \pm -> do
  fpmAddInstructionCombining pm
  fpmAddReassociate pm
  fpmAddGVN pm
  fpmAddCFGSimplification pm
  _ <- fpmInitialize pm
  k pm

main :: IO ()
main = do
  _ <- withContext $ \ctx ->
    withModule "module" ctx $ \mdl ->
      withBuilder ctx $ \bld ->
        genExpr ctx mdl bld mempty (Call "soup" [])
  pure ()
