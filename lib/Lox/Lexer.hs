module Lox.Lexer where
import System.IO (stderr, hPutStrLn, readFile', isEOF)
import Control.Monad (forever, unless, MonadPlus (mzero))
import Control.Monad.Trans.Maybe (MaybeT(runMaybeT))
import Control.Applicative (Alternative((<|>)))

runPrompt :: IO ()
runPrompt = do
  hPutStrLn stderr "Welcome to the Lox interpreter!"
  forever (getLine >>= run) <|> hPutStrLn stderr "See ya!"

runFile :: String -> IO ()
runFile filePath = do
  hPutStrLn stderr ("Received " ++ filePath)
  readFile' filePath >>= run

run :: String -> IO ()
run = putStrLn
