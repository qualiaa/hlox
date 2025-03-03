module Lox.Token where

import Lox.Loc (Loc(..))

               -- Single character tokens
data TokenType = LeftParen
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
               | Identifier
               | String_
               | Number
               -- Keywords
               | And
               | Class
               | Else
               | False_
               | Fun
               | For
               | If
               | Nil
               | Or
               | Print
               | Return
               | Super
               | This
               | True_
               | Var
               | While

               | EOF
               deriving (Eq, Show)

data Token = Token { tokenType   :: !TokenType
                   , tokenLoc    :: !Loc
                   , tokenLexeme :: !String
                   } deriving Show

keyword :: String -> Maybe TokenType
keyword "for"    = Just For
keyword "and"    = Just And
keyword "class"  = Just Class
keyword "else"   = Just Else
keyword "false"  = Just False_
keyword "fun"    = Just Fun
keyword "if"     = Just If
keyword "nil"    = Just Nil
keyword "or"     = Just Or
keyword "print"  = Just Print
keyword "return" = Just Return
keyword "super"  = Just Super
keyword "this"   = Just This
keyword "true"   = Just True_
keyword "var"    = Just Var
keyword "while"  = Just While
keyword _ = Nothing
