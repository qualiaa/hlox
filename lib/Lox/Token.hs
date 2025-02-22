module Lox.Token where

import Lox.Loc (Loc(..))

           -- Single character tokens
data RawToken = LeftParen
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
              | Identifier !String
              | String_    !String
              | Number     !Double
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
              deriving Show

data Token = Token { token :: !RawToken
                   , loc   :: !Loc
                   } deriving Show


lexeme :: RawToken -> String
lexeme     LeftParen  = "("
lexeme     RightParen = ")"
lexeme      LeftBrace = "{"
lexeme     RightBrace = "}"
lexeme          Comma = ","
lexeme            Dot = "."
lexeme          Minus = "-"
lexeme           Plus = "+"
lexeme      Semicolon = ";"
lexeme          Slash = "/"
lexeme           Star = "*"
lexeme           Bang = "!"
lexeme      BangEqual = "!="
lexeme          Equal = "="
lexeme     EqualEqual = "=="
lexeme        Greater = ">"
lexeme   GreaterEqual = ">="
lexeme           Less = "<"
lexeme      LessEqual = "<="
lexeme (Identifier s) = show s
lexeme    (String_ s) = show s
lexeme     (Number n) = show n
lexeme            And = "and"
lexeme          Class = "class"
lexeme           Else = "else"
lexeme         False_ = "false"
lexeme            Fun = "fun"
lexeme            For = "for"
lexeme             If = "if"
lexeme            Nil = "nil"
lexeme             Or = "or"
lexeme          Print = "print"
lexeme         Return = "return"
lexeme          Super = "super"
lexeme           This = "this"
lexeme          True_ = "true"
lexeme            Var = "var"
lexeme          While = "while"
lexeme            EOF = ""

lexLen :: RawToken -> Int
lexLen = length . lexeme
