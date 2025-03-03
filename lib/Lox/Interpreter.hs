module Lox.Interpreter where

import Control.Exception (Exception, throw)
import Control.Monad.Identity (Identity(..))
import Control.Monad.Except (ExceptT(..), runExceptT, tryError, MonadError(throwError), withError)
import Control.Monad.Reader (ReaderT(..), runReaderT)
import Control.Monad.State (StateT(..), evalStateT, runStateT, gets, modify', MonadState (get, put))
import Data.Typeable (Typeable)

import Lox.AST
import Lox.Config
import Lox.Token(Token(..), TokenType(..))

type ProgramState = ()
type RuntimeError = String

type LoxT m = ReaderT LoxConfig (StateT ProgramState (ExceptT RuntimeError m))

newtype EvalException = BadASTException Expr deriving (Show, Typeable)
instance Exception EvalException

runLoxT :: (Monad m) => LoxConfig -> ProgramState -> LoxT m a -> m (Either RuntimeError (a, ProgramState))
runLoxT c s a = let a' = runReaderT a c
                    a'' = runStateT a' s
                in runExceptT a''

evalLoxT c s a = fmap fst <$> runLoxT c s a
execLoxT c s a = fmap snd <$> runLoxT c s a

runLox  c s = runIdentity . runLoxT  c s
evalLox c s = runIdentity . evalLoxT c s
execLox c s = runIdentity . execLoxT c s

eval :: Expr -> Either RuntimeError LoxVal
eval ast = evalLox () () (eval' ast)

eval' :: (Monad m) => Expr -> LoxT m LoxVal
eval' (Literal v) = return v

eval' node@(Unary op expr) = do
  value <- eval' expr
  case tokenType op of
    Minus -> LoxNum . negate <$> toNumber value
    Bang  -> return $ LoxBool (not $ truthiness value)
    _     -> throw $ BadASTException node

eval' node@(Binary left op right) = do
  a <- eval' left
  b <- eval' right

  case tokenType op of
    BangEqual    -> return $ LoxBool (a /= b)
    EqualEqual   -> return $ LoxBool (a == b)
    GreaterEqual -> LoxBool <$> ((>=) <$> toNumber a <*> toNumber b)
    Greater      -> LoxBool <$> ((>)  <$> toNumber a <*> toNumber b)
    LessEqual    -> LoxBool <$> ((<=) <$> toNumber a <*> toNumber b)
    Less         -> LoxBool <$> ((<)  <$> toNumber a <*> toNumber b)
    Star         -> LoxNum  <$> ((*)  <$> toNumber a <*> toNumber b)
    Slash        -> LoxNum  <$> ((/)  <$> toNumber a <*> toNumber b)
    Minus        -> LoxNum  <$> ((-)  <$> toNumber a <*> toNumber b)
    Plus         -> add a b
    _            -> throw $ BadASTException node


eval' (Grouping expr) = eval' expr

truthiness :: LoxVal -> Bool
truthiness (LoxBool b) = b
truthiness (LoxStr _)  = True
truthiness (LoxNum _)  = True
truthiness LoxNil      = False

toNumber :: (MonadError RuntimeError m) => LoxVal -> m Double

toNumber (LoxNum v)      = return v
toNumber (LoxBool True)  = return 1
toNumber (LoxBool False) = return 0

toNumber (LoxStr _) = throwError "Cannot convert string to number"
toNumber LoxNil     = throwError "Cannot convert nil to number"

add :: (MonadError RuntimeError m) => LoxVal -> LoxVal -> m LoxVal
add (LoxStr a) (LoxStr b) = return $ LoxStr (a ++ b)
add (LoxNum a) (LoxNum b) = return $ LoxNum (a + b)
add _ _ =  throwError "Can only add two numbers or two strings"
