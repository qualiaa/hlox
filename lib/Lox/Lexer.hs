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
import System.IO (hPrint, hPutStr, hPutStrLn, readFile', stderr)
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

data LexError = LexError !Loc String
              | UnexpectedCharacter !Loc Char
              | UnterminatedString !Loc !Loc
              deriving Show

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


printErrors :: String -> [LexError] -> IO ()
printErrors _ [] = putErrLn "Unknown error occurred!"
printErrors s errs =
  let errs' = merge $ sortByLoc errs
      errLines = getLines 0 def (map (startLoc . fst) errs') (lines s)
  in mapM_ (uncurry printError) $ zip errs' errLines

  -- TODO: Colours :)
  where sortByLoc :: [LexError] -> [LexError]
        sortByLoc = sortBy (comparing startLoc)

        -- A series of adjacent UnexpectedCharacters can be merged into one error
        merge :: [LexError] -> [(LexError, String)]
        merge [] = []
        merge (x:xs) = reverse $ foldl' joinAdjacent [(x, "")] xs

        joinAdjacent a@((aErr@(UnexpectedCharacter loc0 _), aS):aErrs) err1@(UnexpectedCharacter loc1 c1) =
          if inc (length aS) loc0  `adjacent` loc1 then (aErr, aS ++ [c1]):aErrs else (err1, ""):a
        joinAdjacent a err = (err, ""):a

        getLines :: Int -> Loc -> [Loc] -> [String] -> [(Int, String, Int)]
        getLines _ _ [] _                     = []
        getLines _ _ _ []                     = error "Ran out of lines"
        getLines lineNo pos (loc:locs) (l:ls) =
          let lineEnd = inc (length l + 1) pos
          in if loc > lineEnd then getLines (lineNo + 1) (inc 1 lineEnd) (loc:locs) ls
          else (lineNo, l, steps pos loc)  : getLines lineNo pos locs (l:ls)

        printError :: (LexError, String) -> (Int, String, Int) -> IO ()
        printError (err, extraChars) (lineNo, line, startChar) = do
          let errLen = length extraChars + steps (startLoc err) (endLoc err)
              errLen' = min errLen $ length line - startChar
          hPutStrLn stderr ""
          hPutStrLn stderr $ "  Error " ++ show lineNo ++  ":" ++ show startChar ++ ":"
          hPutStrLn stderr $  "    " ++ line
          hPutStr   stderr $ replicate (startChar + 4) ' '
          hPutStrLn stderr $ errMarker errLen'
          hPutStrLn stderr $ prettyErr err extraChars

        errMarker errLen = '^' : replicate (errLen-1) '~'

        prettyErr (LexError _ s) _ = "Lexical Error: " ++ s
        prettyErr (UnexpectedCharacter _ c) "" = "Unexpected character: '" ++ [c] ++ "'"
        prettyErr (UnexpectedCharacter _ c) s = "Unexpected characters: '" ++ c:s ++ "'"
        prettyErr (UnterminatedString _ _) _ = "Unterminated string"








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

  where lexOneRaw :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m RawToken
        lexOneRaw = do
          startLoc <- gets loc

          openQuote <- tryError $ char '"'
          case openQuote of
            Right _ -> do
              value <- many $ satisfy (/='"')
              squashErrors (char '"' >> return (Tok.String_ value)) <|> unterminatedString startLoc

            Left _ -> squashErrors (asum [
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
             ) <|> unexpectedCharacter


        lexOne :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m Token
        lexOne = do
          -- lexOneRaw concatenates lex errors for every token. We need to
          -- replace that with a single error indicating the unlexable character
          skipNonTokens

          uncurry Tok.Token . swap <$> withLoc lexOneRaw

        findAllErrors :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m a
        findAllErrors = do
          tok <- tryError lexOne

          case tok of
            Right (Tok.Token{token=Tok.EOF}) -> throwError []
            Right _ -> findAllErrors

            Left [] -> lexError "Unknown error!"

            Left [e] -> skipError e >> throwError [e] <|> findAllErrors

            Left e -> throw (PatternMatchFail $ "Compound errors have not been resolved! " ++ show e)

skipSpaces :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m ()
skipSpaces = skipMany $ satisfy isSpace

-- Comment handling: if //, ignore characters until \n or EOF
-- NOTE: This may hit EOF in file with no trailing newline (or repl). This is ok
--       because successful lex of eof is idempotent.
skipNonTokens :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m ()
skipNonTokens = skipSpaces >> optional (comment >> skipSpaces)
  where comment = string "//" >> nextChar `manyTill` (void (char '\n') <|> eof)

-- As per usual, I have immediately found myself writing a scuffed parser
-- combinator ... if only parsec had been invented.
nextChar :: (MonadState LexState m, MonadError [LexError] m) => m Char
nextChar = do

  oldState@( LexState {loc=loc, toLex=toLex} ) <- get

  case toLex of
    []       -> put oldState             >> lexError "EOF"
    (c:rest) -> put (newState loc toLex) >> return c

  where newState loc (c:rest) = LexState { loc=succ loc , toLex=rest}

number :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m Double
number = read <$> numberString
  where digits = some (satisfy isDigit)

        numberString = do
          decimal <- digits
          fractional <- (optional (char '.') >> digits) <|> return "0"
          return $ decimal ++ "." ++ fractional


identifier :: (Alternative m, MonadState LexState m, MonadError [LexError] m) => m RawToken
identifier = do
  str <- (:) <$> satisfy isAlpha <*> many (satisfy isAlphaNum)
  return $ fromMaybe (Tok.Identifier str) (Tok.keyword str)

look :: (MonadState LexState m) => m String
look = gets toLex

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

manyTill :: (Alternative m) => m a -> m b -> m [a]
manyTill p end = (end $> []) <|> (:) <$> p <*> manyTill p end

withLoc :: (MonadState LexState m) => m a -> m (Loc, a)
withLoc p = gets ((,) . loc) <*> p

between :: (Applicative m) => m open -> m close -> m a -> m a
between open close p = open *> p <* close

skipMany :: (Alternative m, MonadState LexState m, MonadError [LexError] m)
         => m a -> m ()
skipMany = void . many

option :: (Alternative m) => a -> m a -> m a
option x p = p <|> pure x

optional :: (Alternative m) => m a -> m ()
optional p = void p <|> pure ()

eof :: (MonadState LexState m, MonadError [LexError] m) => m ()
eof = do
  remaining <- look
  if null remaining then return () else lexError "Expected EOF"

lexError :: (MonadState LexState m, MonadError [LexError] m)
           => String -> m a
lexError s = do
  loc <- gets loc
  throwError [LexError loc s]

skipError :: (MonadState LexState m, MonadError [LexError] m) => LexError -> m ()
skipError e = do
  loc0 <- gets loc
  let loc1 = endLoc e

  modify' (\s@(LexState{toLex}) -> s {
              loc=loc1,
                toLex=drop (steps loc0 loc1) toLex})

unexpectedCharacter :: (MonadState LexState m, MonadError [LexError] m) => m a
unexpectedCharacter = gets (UnexpectedCharacter . loc) <*> nextChar >>= throwError'

unterminatedString :: (MonadState LexState m, MonadError [LexError] m) => Loc -> m a
unterminatedString startLoc = gets (UnterminatedString startLoc . loc) >>= throwError'

throwError' :: (MonadState LexState m, MonadError [LexError] m) => LexError -> m a
throwError' e = throwError [e]

startLoc :: LexError -> Loc
startLoc (LexError l _) = l
startLoc (UnexpectedCharacter l _) = l
startLoc (UnterminatedString l _) = l

endLoc :: LexError -> Loc
endLoc (UnterminatedString _ l) = succ l
endLoc e = succ $ startLoc e

setError :: (MonadError [LexError] m) => LexError -> m a -> m a
setError e = withError $ const [e]

squashErrors :: (MonadError [LexError] m) => m a -> m a
squashErrors = withError $ const []
