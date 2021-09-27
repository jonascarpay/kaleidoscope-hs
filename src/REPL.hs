module REPL where

import AST
import Codegen
import Control.Monad.IO.Class
import Data.ByteString.Char8 qualified as BS8
import LLVM.Bindings (ContextRef, ModuleRef, moduleDump, valueDump)
import Parser (parse)
import System.Console.Haskeline

runRepl :: IO ()
runRepl =
  withContext $ \ctx ->
    withModule "theModule" ctx $ \mdl ->
      runInputT defaultSettings (loop ctx mdl)
  where
    loop :: ContextRef -> ModuleRef -> InputT IO ()
    loop ctx mdl = go
      where
        go = do
          minput <- getInputLine "Vandelay Industries> "
          case minput of
            Nothing -> outputStrLn "Goodbye"
            Just input ->
              case parse (BS8.pack input) of
                Left str -> outputStrLn str >> go
                Right top -> do
                  liftIO $ case top of
                    TLDef def -> genFunction ctx mdl def >> moduleDump mdl
                    TLProto proto -> genProto ctx mdl proto >> moduleDump mdl
                    TLExpr expr -> withBuilder ctx $ \bld -> genExpr ctx mdl bld mempty expr >>= valueDump
                  outputStrLn ""
                  go
