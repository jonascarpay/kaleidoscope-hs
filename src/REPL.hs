module REPL (runRepl) where

import AST
import Codegen
import Control.Monad.IO.Class
import Data.ByteString.Char8 qualified as BS8
import LLVM.Bindings (ContextRef, ModuleRef, PassManagerRef, moduleDump, valueDump)
import Parser (parse)
import System.Console.Haskeline

handleInput :: ContextRef -> ModuleRef -> PassManagerRef -> String -> IO ()
handleInput ctx mdl pm = go
  where
    go "" = moduleDump mdl
    go (':' : cmd) = putStrLn cmd
    go str =
      case parse (BS8.pack str) of
        Left err -> putStrLn err
        Right top -> do
          case top of
            TLDef def -> genFunction ctx mdl pm def >> moduleDump mdl
            TLProto proto -> genProto ctx mdl proto >> moduleDump mdl
            TLExpr expr -> withBuilder ctx $ \bld -> genExpr ctx mdl bld mempty expr >>= valueDump
          putStrLn ""

runRepl :: IO ()
runRepl =
  withContext $ \ctx ->
    withModule "theModule" ctx $ \mdl ->
      withFunctionPassManager mdl $ \pm ->
        runInputT defaultSettings (loop ctx mdl pm)
  where
    loop :: ContextRef -> ModuleRef -> PassManagerRef -> InputT IO ()
    loop ctx mdl pm = go
      where
        go = do
          minput <- getInputLine "Vandelay Industries> "
          case minput of
            Nothing -> outputStrLn "Goodbye"
            Just input -> liftIO (handleInput ctx mdl pm input) >> go
