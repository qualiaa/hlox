{-# LANGUAGE LambdaCase #-}
module Lox.Lexer where

import Control.Monad (forever, foldM_, void)
import Control.Applicative (Alternative((<|>)), asum, some, many)
import Control.Monad.Identity (Identity(..))
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except (ExceptT(..), runExceptT, tryError, MonadError(throwError), withError)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (StateT(..), evalStateT, runStateT, gets, modify', MonadState (get, put))
import Control.Exception (catch, throwIO, throw, PatternMatchFail(..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Default (def)
import Data.Functor (($>))
import Data.List (sortBy, foldl', singleton)
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import Data.Tuple (swap)
import System.IO (hPrint, hPutStrLn, readFile', stderr)
import System.IO.Error (isEOFError)
import Prelude hiding (lex)

import qualified Lox.Token as Tok (RawToken(..), Token(..))
import Lox.Token as Tok (RawToken, Token, token, keyword)

import Lox.Loc (Loc, adjacent, inc, steps)

putErrLn = hPutStrLn stderr


data LexState = LexState { loc   :: !Loc
                         , toLex :: !String
                         } deriving Show

lexInit :: String -> LexState
lexInit = LexState def


type LoxConfig = ()

data LexError = LexError Loc
              | UnexpectedCharacter Loc Char
              -- | UnexpectedRun Loc Loc String  -- Note: store string backwards
              | UnterminatedString Loc Loc
              | ErrorGroup LexError [LexError]

instance Semigroup LexError where
  LexError <> b = b
  a <> LexError = a

  {-
  -- Runs
  a@(UnexpectedCharacter l0 c0) <> b@(UnexpectedCharacter l1 c1)
    | l0 >= l1 = error "Cannot join errors out of order!"
    | adjacent l0 l1 = UnexpectedRun l0 l1 [c0, c1]
    | otherwise = ErrorGroup b [a]

  a@(UnexpectedCharacter l c) <> b@(UnexpectedRun l0 l1 s)
    | l >= l0 = error "Cannot join errors out of order!"
    | adjacent l l0 = UnexpectedRun l l1 (s ++ [c])
    | otherwise = ErrorGroup b [a]

  a@(UnexpectedRun l0 l1 s) <> b@(UnexpectedCharacter l c)
    | l1 >= l = error "Cannot join errors out of order!"
    | adjacent l1 l = UnexpectedRun l0 l (c:s)

  UnexpectedRun l0 l1 s <> UnexpectedRun l0' l1' s'
    | l0' <= l1 = error "Cannot join errors out of order!"
    | adjacent l1 l0' = UnexpectedRun l0 l1' (s' ++ s)
  -}

  -- ErrorGroups
  ErrorGroup x xs <> ErrorGroup y ys = ErrorGroup y (ys ++ (x:xs))

  ErrorGroup x xs <> b = ErrorGroup b (x:xs)
  a <> (ErrorGroup x xs) = ErrorGroup x (xs ++ [a])

  -- Unrelated Errors
  a <> b = a


instance Monoid LexError where
  mempty = LexError


type LoxT m = ReaderT LoxConfig (StateT LexState (ExceptT LexError m))
type Lox = LoxT Identity

runLoxT :: (Monad m) => LoxConfig -> LexState -> LoxT m a -> m (Either LexError (a, LexState))
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
        repl = forever (
          do
            line <- getLine
            let state = LexState { loc=def, toLex = line}

            res <- evalLoxT () state run
            case res of
                Left errors -> printErrors line errors
                Right _ -> return ()
          )

runFile :: String -> IO ()
runFile filePath = do
  putErrLn ("Received " ++ filePath)

  code <- readFile' filePath

  evalLoxT () (lexInit code) run >>= (
    \case
      Left errors -> liftIO $ printErrors code errors
      Right _ -> return ())


printErrors :: String -> LexError -> IO ()
printErrors _ [] = putErrLn "Unknown error occurred!"
--printErrors s errs = (mapM_ (hPrint stderr) . merge . sortByLoc) errs
printErrors s errs = (mapM_ (hPrint stderr) . sortByLoc) errs
  -- TODO: Print source code
  -- TODO: Colours :)
  where sortByLoc :: LexError -> LexError
        sortByLoc = sortBy (comparing startLoc)

        merge :: LexError -> [(LexError, Int)]
        merge [] = []
        merge (x:xs) = reverse $ foldl' joinAdjacent [(x, 0)] (zip (x:xs) xs)

        joinAdjacent a@((aErr, aN):aErrs) (err0, err1) =
          if startLoc err0 `adjacent` startLoc err1 then (aErr, succ aN):aErrs else (err1, 0):a


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

    Left _ -> findAllErrors

  where lexOneRaw :: (Alternative m, MonadState LexState m, MonadError LexError m) => m RawToken
        lexOneRaw = do
          -- This function returns either a RawToken or exactly one LexError.
          -- The error logic is extremely messed up because of how I'm using
          -- transformers.
          startLoc <- gets loc

          -- If we have a " then we are lexing a string, which has its own error
          -- logic; all other tokens use the same error logic.
          let unterminatedString = do {endLoc <- getsLoc; withError' startLoc endLoc "Unterminated string"}

          (Tok.String_ <$> char '"' >> many $ satisfy (/='"') <* (char '"' <|> unterminatedString))
            <|> withError' startLoc startLoc "Unexpected character" $ asum [
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

                char '=' >> (char '=' >> return Tok.EqualEqual)   <|> return Tok.Equal,
                char '!' >> (char '=' >> return Tok.BangEqual)    <|> return Tok.Bang,
                char '>' >> (char '=' >> return Tok.GreaterEqual) <|> return Tok.Greater,
                char '<' >> (char '=' >> return Tok.LessEqual)    <|> return Tok.Less,

                -- Comments have already been dealt with
                char '/' >> return Tok.Slash,

                -- FIXME: Because we parse the number, we need to store the lexeme for rich error reporting.
                Tok.Number <$> number,

                -- Identifier parsing includes keyword tokens
                identifier,

                eof >> return Tok.EOF
                ]


        lexOne :: (Alternative m, MonadState LexState m, MonadError LexError m) => m Token
        lexOne = do
          -- lexOneRaw concatenates lex errors for every token. We need to
          -- replace that with a single error indicating the unlexable character
          skipNonTokens

          uncurry Tok.Token . swap <$> withLoc lexOneRaw

        findAllErrors :: (Alternative m, MonadState LexState m, MonadError LexError m) => m a
        findAllErrors = do
          tok <- tryError lexOne

          let skipError e = do
              modify' (\s@(LexState{toLex}) -> s {
                          loc=endLoc,
                          toLex=drop (steps startLoc endLoc) toLex})
              errorSteps e = do
                startLoc <- gets loc
                case e of
                  LexError loc -> steps startLoc loc
                  UnexpectedCharacter loc -> steps startLoc loc
                  UnterminatedString _ loc -> steps startLoc loc
                  ErrorGroup e _ -> errorSteps e

          case tok of
            Right (Tok.Token{token=Tok.EOF}) -> throwError LexError
            Right _ -> findAllErrors

            Left group@(ErrorGroup e es) -> skipError e >> throwError group <|> findAllErrors
            Left e -> skipError x >> throwError (ErrorGroup e [])           <|> findAllErrors

skipSpaces :: (Alternative m, MonadState LexState m, MonadError LexError m) => m ()
skipSpaces = skipMany $ satisfy isSpace

-- Comment handling: if //, ignore characters until \n or EOF
-- NOTE: This may hit EOF in file with no trailing newline (or repl). This is ok
--       because successful lex of eof is idempotent.
skipNonTokens :: (Alternative m, MonadState LexState m, MonadError LexError m) => m ()
skipNonTokens = skipSpaces >> optional (comment >> skipSpaces)
  where comment = string "//" >> nextChar `manyTill` (void (char '\n') <|> eof)

-- As per usual, I have immediately found myself writing a scuffed parser
-- combinator ... if only parsec had been invented.
nextChar :: (MonadState LexState m, MonadError LexError m) => m Char
nextChar = do

  oldState@( LexState {loc=loc, toLex=toLex} ) <- get

  case toLex of
    []       -> put oldState             >> lexError "EOF"
    (c:rest) -> put (newState loc toLex) >> return c

  where newState loc (c:rest) = LexState { loc=succ loc , toLex=rest}

number :: (Alternative m, MonadState LexState m, MonadError LexError m) => m Double
number = read <$> numberString
  where digits = some (satisfy isDigit)

        numberString = do
          decimal <- digits
          fractional <- (optional (char '.') >> digits) <|> return "0"
          return $ decimal ++ "." ++ fractional


identifier :: (Alternative m, MonadState LexState m, MonadError LexError m) => m RawToken
identifier = do
  str <- (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
  return $ fromMaybe (Tok.Identifier str) (Tok.keyword str)

look :: (MonadState LexState m) => m String
look = gets toLex

char :: (MonadState LexState m, MonadError LexError m) => Char -> m Char
char c = do
  c' <- nextChar
  if c == c' then return c else lexError $ "Expected " ++ [c] ++ "but found" ++ [c']

string :: (MonadState LexState m, MonadError LexError m) => String -> m String
string = mapM char

satisfy :: (MonadState LexState m, MonadError LexError m) => (Char -> Bool) -> m Char
satisfy p = do
  c <- nextChar
  if p c then return c else lexError $ "Character" ++ [c] ++ "did not match predicate"

manyTill :: (Alternative m) => m a -> m b -> m [a]
manyTill p end = (end $> []) <|> (:) <$> p <*> manyTill p end

withLoc :: (MonadState LexState m) => m a -> m (Loc, a)
withLoc p = gets ((,) . loc) <*> p

between :: (Applicative m) => m open -> m close -> m a -> m a
between open close p = open *> p <* close

skipMany :: (Alternative m, MonadState LexState m, MonadError LexError m)
         => m a -> m ()
skipMany = void . many

option :: (Alternative m) => a -> m a -> m a
option x p = p <|> pure x

optional :: (Alternative m) => m a -> m ()
optional p = void p <|> pure ()

eof :: (MonadState LexState m, MonadError LexError m) => m ()
eof = do
  remaining <- look
  if null remaining then return () else lexError "Expected EOF"

lexError :: (MonadState LexState m, MonadError LexError m)
           => String -> m a
lexError s = do
  loc <- gets loc
  throwError [LexError loc (succ loc) s]

withError' :: MonadError LexError m => Loc -> Loc -> String -> m a -> m a
withError' start end msg = withError (const [LexError start (succ end) msg])
