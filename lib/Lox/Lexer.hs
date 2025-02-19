{-# LANGUAGE LambdaCase #-}
module Lox.Lexer where
import Control.Monad (forever)
import Control.Applicative (Alternative((<|>)))
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT(..), runExceptT)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (StateT(..), runStateT)
import Control.Exception (catch, throwIO)
import System.IO (hPutStrLn, readFile', stderr)
import System.IO.Error (isEOFError)
import Prelude hiding (lex)

type LoxConfig = ()
type LoxState = ()

type LoxT m = ExceptT String (ReaderT LoxConfig (StateT LoxState m))
type Lox = LoxT Identity

runLoxT :: (Monad m) => LoxConfig -> LoxState -> LoxT m a -> m (Either String a, LoxState)
runLoxT c s a = let a'  = runExceptT a
                    a'' = runReaderT a' c
               in runStateT a'' s

evalLoxT c s = fmap fst . runLoxT c s
execLoxT c s = fmap snd . runLoxT c s

runLox  c s a = let (Identity x) = runLoxT  c s a in x
evalLox c s a = let (Identity x) = evalLoxT c s a in x
execLox c s a = let (Identity x) = execLoxT c s a in x

runPrompt :: IO ()
runPrompt = do
  liftIO $ hPutStrLn stderr "Welcome to the Lox interpreter!"

  repl `catch` \(e :: IOError) -> if isEOFError e then hPutStrLn stderr "See ya!"
                                  else throwIO e

  where repl = forever (
          do
            line <- getLine
            evalLoxT () () (run line) >>= (
              \case
                Left err -> hPutStrLn stderr err
                Right _ -> return ()))


runFile :: String -> IO ()
runFile filePath = do
  hPutStrLn stderr ("Received " ++ filePath)

  code <- readFile' filePath

  evalLoxT () () (run code) >>= (
    \case
      Left err -> hPutStrLn stderr err
      Right _ -> return ())


run :: String -> LoxT IO ()
run source = do
  tokens <- lex source
  liftIO $ mapM_ print tokens

type Token = ()

lex :: (Monad m) => String -> LoxT m [Token]
lex _ = return [()]
