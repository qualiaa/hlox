module Main where

import Lox.Lexer ( runFile, runPrompt )
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode (ExitFailure))

main :: IO ()
main = do
  args <- getArgs
  case args of
    [] -> runPrompt
    [filePath] -> runFile filePath
    _tooManyArgs -> do
      getProgName >>= (\progName -> hPutStrLn stderr ("Usage: " ++ progName ++ " [script]"))
      exitWith (ExitFailure 64)
