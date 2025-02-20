module Lox.Token where

import Lox.Loc (Loc(..))

           -- Single character tokens
data Token = LeftParen
           | RightParen
           | LeftBrace
           | RightBrace
           | Comma
           | Dot
           | Minus
           | Plus
           | Semicolon
           | Slash
           | Star
           -- One or two character tokens
           | Bang
           | BangEqual
           | Equal
           | EqualEqual
           | Greater
           | GreaterEqual
           | Less
           | LessEqual
           -- Literals
           | Identifier String
           | String_ String
           | Number Double
           -- Keywords
           | And
           | Class
           | Else
           | False
           | Fun
           | For
           | If
           | Nil
           | Or
           | Print
           | Return
           | Super
           | This
           | True
           | Var
           | While

           | EOF
           deriving Show

data LabelledToken = LabelledToken{ token :: Token
                                  , loc :: Loc
                                  , lexeme :: String
                                  } deriving Show
