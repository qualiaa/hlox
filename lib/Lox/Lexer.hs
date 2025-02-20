{-# LANGUAGE LambdaCase #-}
module Lox.Lexer where

import Control.Monad (forever)
import Control.Applicative (Alternative((<|>)))
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT(..), runExceptT, throwError)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (StateT(..), runStateT)
import Control.Exception (catch, throwIO)
import System.IO (hPutStrLn, readFile', stderr)
import System.IO.Error (isEOFError)
import Prelude hiding (lex)
import Data.Default (Default, def)

import qualified Lox.Token as Tok (Token(..), LabelledToken(..))
import Lox.Token as Tok (Token, LabelledToken)

import Lox.Loc (Loc(..))

newtype LoxState = LoxState { loc :: Loc }

instance Default LoxState where
  def = LoxState { loc = mempty }

type LoxConfig = ()


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
            evalLoxT () def (run line) >>= (
              \case
                Left err -> hPutStrLn stderr err
                Right _ -> return ()))


runFile :: String -> IO ()
runFile filePath = do
  hPutStrLn stderr ("Received " ++ filePath)

  code <- readFile' filePath

  evalLoxT () def (run code) >>= (
    \case
      Left err -> hPutStrLn stderr err
      Right _ -> return ())


run :: String -> LoxT IO ()
run source = do
  tokens <- lex source
  liftIO $ mapM_ print tokens

lex :: (Monad m) => String -> LoxT m [LabelledToken]
lex s = reverse <$> go [] s

  where lexMap :: String -> Maybe (String, Token, String)
        lexMap ('(':s) = Just ("(", Tok.LeftParen, s)
        lexMap (')':s) = Just (")", Tok.RightParen, s)
        lexMap ('{':s) = Just ("{", Tok.LeftBrace, s)
        lexMap ('}':s) = Just ("}", Tok.RightBrace, s)
        lexMap (',':s) = Just (",", Tok.Comma, s)
        lexMap ('.':s) = Just (".", Tok.Dot, s)
        lexMap ('-':s) = Just ("-", Tok.Minus, s)
        lexMap ('+':s) = Just ("+", Tok.Plus, s)
        lexMap (';':s) = Just (";", Tok.Semicolon, s)
        lexMap ('*':s) = Just ("*", Tok.Star, s)
        lexMap _       = Nothing

        -- Horrible hack, will redo this imminently
        label t = Tok.LabelledToken t mempty
        go lexed input = do
          case lexMap input of
            Nothing        -> throwError "Bad times!"
            Just (parsed, t, [])   -> return (label t parsed:lexed)
            Just (parsed, t, rest) -> go (label t parsed:lexed) rest
