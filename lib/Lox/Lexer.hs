module Lox.Lexer where

import Control.Monad (void)
import Control.Applicative (Alternative((<|>)), asum, some, many)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Except (ExceptT(..), runExceptT, tryError, MonadError(throwError), withError)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (StateT(..), evalStateT, runStateT, gets, modify', MonadState (get, put))
import Control.Exception (throw, PatternMatchFail(..))
import Data.Char (isAlpha, isAlphaNum, isDigit, isSpace)
import Data.Default (def)
import Data.List (singleton)
import Data.Functor (($>))
import Data.Maybe (fromMaybe)
import Prelude hiding (lex)

import Lox.Token as Tok (TokenType, Token(tokenType), keyword)
import Lox.Loc (Loc, adjacent, inc, steps)


import Lox.Config
import qualified Lox.Token as Tok (TokenType(..), Token(..))

data LexState = LexState { loc   :: !Loc
                         , toLex :: !String
                         } deriving Show

lexInit :: String -> LexState
lexInit = LexState def

data LexError = LexError !Loc String
              | UnexpectedCharacter !Loc Char
              | UnterminatedString !Loc !Loc
              deriving Show

startLoc :: LexError -> Loc
startLoc (LexError l _) = l
startLoc (UnexpectedCharacter l _) = l
startLoc (UnterminatedString l _) = l

endLoc :: LexError -> Loc
endLoc (UnterminatedString _ l) = succ l
endLoc e = succ $ startLoc e

type LexT m = ReaderT LoxConfig (StateT LexState (ExceptT [LexError] m))
type Lex = LexT Identity

runLexT :: (Monad m) => LoxConfig -> LexState -> LexT m a -> m (Either [LexError] (a, LexState))
runLexT c s a = let a'  = runReaderT a c
                    a'' = runStateT a' s
                in runExceptT a''

evalLexT c s = fmap (fmap fst) . runLexT c s
execLexT c s = fmap (fmap snd) . runLexT c s

runLex  c s a = let (Identity x) = runLexT  c s a in x
evalLex c s a = let (Identity x) = evalLexT c s a in x
execLex c s a = let (Identity x) = execLexT c s a in x


lex :: LoxConfig -> String -> Either [LexError] [Token]
lex c s = evalLex c (lexInit s) lex'


lex' :: (Monad m) => LexT m [Token]
lex' = do
  tok <- tryError lexOne
  case tok of
    Right tok@(Tok.Token{tokenType=Tok.EOF}) -> return [tok]
    Right tok -> (tok:) <$> lex'

    Left _ -> findAllErrors

  where lexOneRaw :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m (TokenType, String)
        lexOneRaw = do
          let pair' t = (t,) . singleton
              oneOrTwo t0 c0 t1 c1 =
                char c0 >> (((t1,) . (c0:) . singleton <$> char c1) <|> return (t0, [c0]))

          startLoc <- gets loc
          openQuote <- tryError $ char '"'

          case openQuote of
            -- This lexeme was a string
            Right _ -> do
              value <- many $ satisfy (/='"')
              squashErrors (char '"' >> return (Tok.String_, '"': value ++ ['"'])) <|> unterminatedString startLoc

            -- This lexeme was not a string, try everything else
            Left _ -> squashErrors (asum [
                pair' Tok.LeftParen  <$> char '(',
                pair' Tok.RightParen <$> char ')',
                pair' Tok.LeftBrace  <$> char '{',
                pair' Tok.RightBrace <$> char '}',
                pair' Tok.Comma      <$> char ',',
                pair' Tok.Dot        <$> char '.',
                pair' Tok.Minus      <$> char '-',
                pair' Tok.Plus       <$> char '+',
                pair' Tok.Semicolon  <$> char ';',
                pair' Tok.Star       <$> char '*',
                -- Comments have already been dealt with
                pair' Tok.Slash      <$> char '/',

                oneOrTwo Tok.Equal    '=' Tok.EqualEqual    '=',
                oneOrTwo Tok.Bang     '!' Tok.BangEqual     '=',
                oneOrTwo Tok.Greater  '>' Tok.GreaterEqual  '=',
                oneOrTwo Tok.Less     '<' Tok.LessEqual     '=',

                (Tok.Number,) <$> number,

                -- Identifier lexing includes keyword tokens
                identifier,

                eof >> return (Tok.EOF, "")
                ]
             ) <|> unexpectedCharacter


        lexOne :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m Token
        lexOne = let toToken (loc, (tok, s)) = Tok.Token tok loc s

                 in skipNonTokens >> toToken <$> withLoc lexOneRaw

        findAllErrors :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m a
        findAllErrors = do
          tok <- tryError lexOne

          case tok of
            Right (Tok.Token{tokenType=Tok.EOF}) -> throwError []
            Right _ -> findAllErrors

            Left [] -> lexError "Unknown error!"

            Left [e] -> skipError e >> throwError' e <|> findAllErrors

            Left e -> throw (PatternMatchFail $ "Compound errors have not been resolved! " ++ show e)

number :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m String
number = do
  decimal <- digits
  fractional <- (optional (char '.') >> digits) <|> return "0"
  return $ decimal ++ "." ++ fractional

  where digits = some (satisfy isDigit)


identifier :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m (TokenType, String)
identifier = do
  str <- (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
  return . (, str) $ fromMaybe Tok.Identifier (Tok.keyword str)

-- Comment handling: if //, ignore characters until \n or EOF
-- NOTE: This may hit EOF in file with no trailing newline (or repl). This is ok
--       because successful lex of eof is idempotent.
skipNonTokens :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m ()
skipNonTokens = skipSpaces >> optional (comment >> skipNonTokens)
  where comment = string "//" >> nextChar `manyTill` (void (char '\n') <|> eof)

unexpectedCharacter :: (MonadState LexState m, MonadError [LexError] m) => m a
unexpectedCharacter = gets (UnexpectedCharacter . loc) <*> nextChar >>= throwError'

unterminatedString :: (MonadState LexState m, MonadError [LexError] m) => Loc -> m a
unterminatedString startLoc = gets (UnterminatedString startLoc . loc) >>= throwError'


-- As per usual, I have immediately found myself writing a scuffed parser
-- combinator ... if only parsec had been invented.
nextChar :: (MonadState LexState m, MonadError [LexError] m) => m Char
nextChar = do

  oldState@( LexState {loc=loc, toLex=toLex} ) <- get

  case toLex of
    []       -> put oldState             >> lexError "EOF"
    (c:rest) -> put (newState loc toLex) >> return c

  where newState loc (c:rest) = LexState { loc=succ loc , toLex=rest}

look :: (MonadState LexState m) => m String
look = gets toLex

eof :: (MonadState LexState m, MonadError [LexError] m) => m ()
eof = do
  remaining <- look
  if null remaining then return () else lexError "Expected EOF"

char :: (MonadState LexState m, MonadError [LexError] m) => Char -> m Char
char c = do
  c' <- nextChar
  if c == c' then return c else lexError $ "Expected " ++ [c] ++ "but found" ++ [c']

string :: (MonadState LexState m, MonadError [LexError] m) => String -> m String
string = mapM char

satisfy :: (MonadState LexState m, MonadError [LexError] m) => (Char -> Bool) -> m Char
satisfy p = do
  c <- nextChar
  if p c then return c else lexError $ "Character" ++ [c] ++ "did not match predicate"

skipSpaces :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m ()
skipSpaces = skipMany $ satisfy isSpace

manyTill :: Alternative m => m a -> m b -> m [a]
manyTill p end = (end $> []) <|> (:) <$> p <*> manyTill p end

withLoc :: MonadState LexState m => m a -> m (Loc, a)
withLoc p = (,) <$> gets loc <*> p

between :: Applicative m => m open -> m close -> m a -> m a
between open close p = open *> p <* close

skipMany :: Alternative m => m a -> m ()
skipMany = void . many

option :: Alternative m => a -> m a -> m a
option x p = p <|> pure x

optional :: Alternative m => m a -> m ()
optional p = void p <|> pure ()

lexError :: (MonadState LexState m, MonadError [LexError] m)
           => String -> m a
lexError s = do
  loc <- gets loc
  throwError [LexError loc s]

skipError :: MonadState LexState m => LexError -> m ()
skipError e = do
  loc0 <- gets loc
  let loc1 = endLoc e

  modify' (\s@(LexState{toLex}) -> s {
              loc=loc1,
                toLex=drop (steps loc0 loc1) toLex})

throwError' :: MonadError [e] m => e -> m a
throwError' e = throwError [e]

setError :: MonadError [e] m => e -> m a -> m a
setError e = withError $ const [e]

squashErrors :: MonadError [e] m => m a -> m a
squashErrors = withError $ const []
