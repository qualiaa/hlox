{-# LANGUAGE LambdaCase #-}
module Lox.Lexer where

import Control.Monad (forever, foldM_)
import Control.Applicative (Alternative((<|>)), asum)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT(..), runExceptT, tryError, MonadError(throwError), withError)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (StateT(..), runStateT, gets, MonadState (get, put))
import Control.Exception (catch, throwIO)
import Data.Default (def)
import Data.Tuple (swap)
import System.IO (hPutStrLn, readFile', stderr)
import System.IO.Error (isEOFError)
import Prelude hiding (lex)

import qualified Lox.Token as Tok (Token(..), LabelledToken(..))
import Lox.Token as Tok (Token, LabelledToken)

import Lox.Loc (Loc(..), newLine, nextCol)

data LoxState = LoxState { loc   :: !Loc
                         , toLex :: !String
                         } deriving Show

lexInit :: String -> LoxState
lexInit = LoxState def


type LoxConfig = ()


type LoxT m = ReaderT LoxConfig (StateT LoxState (ExceptT String m))
type Lox = LoxT Identity

--runLoxT :: (Monad m) => LoxConfig -> LoxState -> LoxT m a -> m (Either String a, LoxState)
--runLoxT c s a = let a'  = runExceptT a
--                    a'' = runReaderT a' c
--               in runStateT a'' s
runLoxT :: (Monad m) => LoxConfig -> LoxState -> LoxT m a -> m (Either String (a, LoxState))
runLoxT c s a = let a'  = runReaderT a c
                    a'' = runStateT a' s
                in runExceptT a''

evalLoxT c s = fmap (fmap fst) . runLoxT c s
execLoxT c s = fmap (fmap snd) . runLoxT c s

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
            evalLoxT () (lexInit line) run >>= (
              \case
                Left err -> hPutStrLn stderr err
                Right _ -> return ()))


runFile :: String -> IO ()
runFile filePath = do
  hPutStrLn stderr ("Received " ++ filePath)

  code <- readFile' filePath

  evalLoxT () (lexInit code) run >>= (
    \case
      Left err -> hPutStrLn stderr err
      Right _ -> return ())


run :: LoxT IO ()
run = do
  tokens <- lex
  liftIO $ mapM_ print tokens

lex :: (Monad m) => LoxT m [LabelledToken]
lex = map (uncurry Tok.LabelledToken . swap) <$> endBy lexOneWithError eof

  where lexOne :: (Monad m) => LoxT m (Loc, Token)
        lexOne = asum $ map withLoc [
          char '(' >> return Tok.LeftParen,
          char ')' >> return Tok.RightParen,
          char '{' >> return Tok.LeftBrace,
          char '}' >> return Tok.RightBrace,
          char ',' >> return Tok.Comma,
          char '.' >> return Tok.Dot,
          char '-' >> return Tok.Minus,
          char '+' >> return Tok.Plus,
          char ';' >> return Tok.Semicolon,
          char '*' >> return Tok.Star
          ]

        lexOneWithError :: (Monad m) => LoxT m (Loc, Token)
        lexOneWithError = do
          loc <- gets loc
          upcoming <- take 1 <$> look
          withError (const $ "Unexpected character at " ++ show loc ++ "; got: " ++ upcoming) lexOne




-- As per usual, I have immediately found myself writing a scuffed parser
-- combinator ... if only parsec had been invented.
nextChar :: (MonadState LoxState m, MonadError String m) => m Char
nextChar = do

  oldState@( LoxState {loc=loc, toLex=toLex} ) <- get

  case toLex of
    []       -> put oldState             >> throwError "EOF"
    (c:rest) -> put (newState loc toLex) >> return c

  where newState loc (c:rest) = LoxState { loc=if c == '\n'
                                               then newLine loc
                                               else nextCol loc
                                         , toLex=rest
                                         }

look :: (MonadState LoxState m) => m String
look = gets toLex

char :: (MonadState LoxState m, MonadError String m) => Char -> m Char
char c = do
  c' <- nextChar
  if c == c' then return c else throwError $ "Expected " ++ [c] ++ "but found" ++ [c']

withLoc :: (MonadState LoxState m, MonadError String m) => m a -> m (Loc, a)
withLoc p = gets ((,) . loc) <*> p

many :: (MonadState LoxState m, MonadError String m) => m a -> m [a]
many p = do
  res <-  tryError p
  case res of
    Right v -> (v:) <$> many p
    Left _ -> return []

some :: (MonadState LoxState m, MonadError String m) => m a -> m [a]
some p = (:) <$> p <*> many p

endBy :: (MonadState LoxState m, MonadError String m) => m a -> m b ->  m [a]
endBy p e = do
  lexed <- some p
  loc <- gets loc
  upcoming <- take 1 <$> look
  withError (\err -> err ++ " at " ++ show loc ++"; got: " ++ upcoming) e
  return lexed

eof :: (MonadState LoxState m, MonadError String m) => m ()
eof = do
  remaining <- gets toLex
  if null remaining then return () else throwError "Expected EOF"

--str :: (MonadState LoxState m, MonadError String m) => String -> m String
--str s = foldM_
