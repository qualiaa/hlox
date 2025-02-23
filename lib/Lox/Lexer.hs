{-# LANGUAGE LambdaCase #-}
module Lox.Lexer where

import Control.Monad (forever, foldM_, void)
import Control.Applicative (Alternative((<|>)), asum)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT(..), runExceptT, tryError, MonadError(throwError), withError)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (StateT(..), evalStateT, runStateT, gets, modify', MonadState (get, put))
import Control.Exception (catch, throwIO)
import Data.Default (def)
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import Data.Tuple (swap)
import System.IO (hPrint, hPutStrLn, readFile', stderr)
import System.IO.Error (isEOFError)
import Prelude hiding (lex)

import qualified Lox.Token as Tok (RawToken(..), Token(..))
import Lox.Token as Tok (RawToken, Token, token)

import Lox.Loc (Loc(..), adjacent, newLine, nextCol)
import Data.List (singleton)


putErrLn = hPutStrLn stderr


data LexState = LexState { loc   :: !Loc
                         , toLex :: !String
                         } deriving Show

lexInit :: String -> LexState
lexInit = LexState def


type LoxConfig = ()

data LexError = LexError { errLoc :: !Loc
                         , errString :: !String
                         } deriving Show

type LoxT m = ReaderT LoxConfig (StateT LexState (ExceptT [LexError] m))
type Lox = LoxT Identity

runLoxT :: (Monad m) => LoxConfig -> LexState -> LoxT m a -> m (Either [LexError] (a, LexState))
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
  liftIO $ putErrLn "Welcome to the Lox interpreter!"

  repl `catch` \(e :: IOError) -> if isEOFError e then putErrLn "See ya!"
                                  else throwIO e

  where repl :: IO ()
        repl = evalStateT repl' 0

        repl' :: StateT Int IO ()
        repl' = forever (
          do
            lineNo <- get
            line <- liftIO getLine
            let state = LexState { loc=Loc {codeLine=lineNo, codeCol=0}
                                 , toLex = line
                                 }

            res <- liftIO $ evalLoxT () state run
            case res of
                Left errors -> liftIO $ printErrors line errors
                Right _ -> void $ modify' (+1)
          )

runFile :: String -> IO ()
runFile filePath = do
  putErrLn ("Received " ++ filePath)

  code <- readFile' filePath

  evalLoxT () (lexInit code) run >>= (
    \case
      Left errors -> liftIO $ printErrors code errors
      Right _ -> return ())


printErrors :: String -> [LexError] -> IO ()
printErrors _ [] = putErrLn "Unknown error occurred!"
printErrors s errs = (mapM_ (hPrint stderr) . merge . sortByLoc) errs
  -- TODO: Print source code
  -- TODO: Colours :)
  where sortByLoc :: [LexError] -> [LexError]
        sortByLoc = sortBy (comparing ((\(Loc{codeLine, codeCol}) -> (codeLine, codeCol)) . errLoc))

        merge :: [LexError] -> [(LexError, Int)]
        merge [] = []
        merge (x:xs) = reverse $ foldl' joinAdjacent [(x, 0)] (zip (x:xs) xs)

        joinAdjacent a@((aErr, aN):aErrs) (err0, err1) =
          if errLoc err0 `adjacent` errLoc err1 then (aErr, succ aN):aErrs else (err1, 0):a


run :: LoxT IO ()
run = do
  tokens <- lex
  liftIO $ mapM_ print tokens

lex :: (Monad m) => LoxT m [Token]
lex = do
  tok <- tryError lexOne
  case tok of
    Right tok@(Tok.Token{token=Tok.EOF}) -> return [tok]
    Right tok -> (tok:) <$> lex

    Left e -> nextChar >> withError (e++) findAllErrors

  where lexOneRaw :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m (Loc, RawToken)
        lexOneRaw = asum $ map withLoc [
          char '(' >> return Tok.LeftParen,
          char ')' >> return Tok.RightParen,
          char '{' >> return Tok.LeftBrace,
          char '}' >> return Tok.RightBrace,
          char ',' >> return Tok.Comma,
          char '.' >> return Tok.Dot,
          char '-' >> return Tok.Minus,
          char '+' >> return Tok.Plus,
          char ';' >> return Tok.Semicolon,
          char '*' >> return Tok.Star,
          eof      >> return Tok.EOF
          ]

        lexOne :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m Token
        lexOne = do
          err <- gets (LexError . loc) <*> (take 1 <$> look)
          uncurry Tok.Token . swap <$> withError (const [err]) lexOneRaw

        findAllErrors :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m a
        findAllErrors = do
          tok <- tryError lexOne
          case tok of
            Right tok@(Tok.Token{token=Tok.EOF}) -> throwError []
            Right _ -> findAllErrors
            Left e -> throwError e <|> (nextChar >> findAllErrors)



-- As per usual, I have immediately found myself writing a scuffed parser
-- combinator ... if only parsec had been invented.
nextChar :: (MonadState LexState m, MonadError [LexError] m) => m Char
nextChar = do

  oldState@( LexState {loc=loc, toLex=toLex} ) <- get
  let newState c toLex = LexState { loc=if c == '\n'
                                    then newLine loc
                                    else nextCol loc
                                  , toLex=tail toLex
                                  }

  case toLex of
    []         -> put oldState           >> lexError "EOF"
    (c:toLex)  -> put (newState c toLex) >> return c



look :: (MonadState LexState m) => m String
look = gets toLex

char :: (MonadState LexState m, MonadError [LexError] m) => Char -> m Char
char c = do
  c' <- nextChar
  if c == c' then return c else lexError $ "Expected " ++ [c] ++ "but found" ++ [c']

withLoc :: (MonadState LexState m) => m a -> m (Loc, a)
withLoc p = gets ((,) . loc) <*> p

many :: (MonadState LexState m, MonadError [LexError] m) => m a -> m [a]
many p = do
  res <-  tryError p
  case res of
    Right v -> (v:) <$> many p
    Left _ -> return []

some :: (MonadState LexState m, MonadError [LexError] m) => m a -> m [a]
some p = (:) <$> p <*> many p

eof :: (MonadState LexState m, MonadError [LexError] m) => m ()
eof = do
  remaining <- look
  if null remaining then return () else lexError "Expected EOF"

lexError :: (MonadState LexState m, MonadError [LexError] m)
           => String -> m a
lexError s = gets (singleton . flip LexError s . loc) >>= throwError
