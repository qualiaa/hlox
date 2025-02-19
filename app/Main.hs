module Main where

import Control.Applicative (Alternative((<|>)))
import Lox.Lexer ( runFile, runPrompt )
import System.Environment (getArgs, getProgName)
import System.IO (hPutStrLn, stderr)
import System.Exit (exitWith, ExitCode (ExitFailure))

main :: IO ()
main = do
  args <- getArgs
  case args of
    []           -> runPrompt        <|> exitWith (ExitFailure 65)

    [filePath]   -> runFile filePath <|> exitWith (ExitFailure 65)

    _tooManyArgs -> do
      getProgName >>= (\progName -> hPutStrLn stderr ("Usage: " ++ progName ++ " [script]"))
      exitWith (ExitFailure 64)
