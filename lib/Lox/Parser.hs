module Lox.Parser where

import Control.Applicative (Alternative((<|>)), asum)
import Control.Monad (when)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Except (ExceptT(..), runExceptT, MonadError(throwError))
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (StateT(..), evalStateT, runStateT, gets, modify', MonadState (get, put))
import Control.Monad.Trans.Maybe (MaybeT(..))
import Control.Monad.Trans.Class (lift)
import Data.List (foldl')
import Data.List.NonEmpty (NonEmpty((:|)))
import Data.Maybe (isJust, maybe)

import qualified Data.List.NonEmpty as NE

import Lox.AST (Expr(..), LoxVal(..))
import Lox.Config
import Lox.Token (Token(..), TokenType(..))

type ParseState = [Token]
type ParseError = String

type ParseT m = ReaderT LoxConfig (ExceptT ParseError (StateT ParseState m))
type Parse = ParseT Identity

runParseT :: (Monad m) => LoxConfig -> ParseState -> ParseT m a -> m (Either ParseError a, ParseState)
runParseT c s a = let a' = runReaderT a c
                      a'' = runExceptT a'
                  in runStateT a'' s

evalParseT c s a = fst <$> runParseT c s a
execParseT c s a = snd <$> runParseT c s a

runParse  c s = runIdentity . runParseT  c s
evalParse c s = runIdentity . evalParseT c s
execParse c s = runIdentity . execParseT c s

expression :: Parse Expr
expression =
  let binOperators = [ BangEqual :| [EqualEqual]
                     , Less :| [LessEqual, Greater, GreaterEqual]
                     , Minus :| [Plus]
                     , Star :| [Slash]
                     ]
  in foldl' binOp unary $ reverse binOperators

parse :: [Token] -> Either ParseError Expr
parse ts = evalParse () ts expression

unary  :: Parse Expr
unary = do
  m <- match (Bang :| [Minus])
  case m of
    Just tok -> Unary tok <$> unary
    Nothing -> primary


require :: Maybe a -> Parse a
require Nothing = throwError mempty
require (Just a) = return a

expect :: NonEmpty TokenType -> ParseError -> Parse Token
expect ts e = (match ts >>= require) <|> throwError e

primary :: Parse Expr
primary = do
  m <- runMaybeT $ do
    let match' = MaybeT . match . NE.singleton

        literals = map run handlers
        literal = Literal <$> asum literals
        handlers = [ (True_,       const $ LoxBool True)
                   , (False_,      const $ LoxBool False)
                   , (Nil,         const LoxNil)
                   , (String_,     LoxStr . tokenLexeme)
                   , (Number,      LoxNum . read . tokenLexeme)]
        run :: (TokenType, Token -> LoxVal) -> MaybeT Parse LoxVal
        run (t, h) = h <$> match' t

        parenExpr =
          match' LeftParen >> lift (expression <* expect (NE.singleton RightParen) parenErr)

        parenErr = "Expect ')' after expression"

    literal <|> parenExpr

  case m of
    Just expr -> return expr
    Nothing -> throwError "Expected expression."

binOp :: (MonadState ParseState m) => m Expr -> NonEmpty TokenType -> m Expr
binOp operand validOperators = do
  first <- operand

  m <- match validOperators
  case m of
    Just operator -> Binary first operator <$> binOp operand validOperators
    Nothing -> return first

nextToken :: (MonadState ParseState m) => m ()
nextToken = modify' (drop 1)

atEnd :: (MonadState ParseState m) => m Bool
atEnd = gets null

match :: (MonadState ParseState m) => NonEmpty TokenType -> m (Maybe Token)
match ts = do
  matches <- gets match'
  when (isJust matches) nextToken
  return matches
  where match' [] = Nothing
        match' (t:_) = if tokenType t `elem` ts then Just t else Nothing
