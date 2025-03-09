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
               deriving (Eq, Show, Enum, Bounded)

data Token = Token { tokenType   :: !TokenType
                   , tokenLoc    :: !Loc
                   , tokenLexeme :: !String
                   } deriving (Eq, Show)

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


tokenToLexeme :: TokenType -> Maybe String
tokenToLexeme Identifier   = Nothing
tokenToLexeme String_      = Nothing
tokenToLexeme Number       = Nothing
tokenToLexeme LeftParen    = Just "("
tokenToLexeme RightParen   = Just ")"
tokenToLexeme LeftBrace    = Just "{"
tokenToLexeme RightBrace   = Just "}"
tokenToLexeme Comma        = Just ","
tokenToLexeme Dot          = Just "."
tokenToLexeme Minus        = Just "-"
tokenToLexeme Plus         = Just "+"
tokenToLexeme Semicolon    = Just ";"
tokenToLexeme Slash        = Just "/"
tokenToLexeme Star         = Just "*"
tokenToLexeme Bang         = Just "!"
tokenToLexeme BangEqual    = Just "!="
tokenToLexeme Equal        = Just "="
tokenToLexeme EqualEqual   = Just "=="
tokenToLexeme Greater      = Just ">"
tokenToLexeme GreaterEqual = Just ">="
tokenToLexeme Less         = Just "<"
tokenToLexeme LessEqual    = Just "<="
tokenToLexeme And          = Just "and"
tokenToLexeme Class        = Just "class"
tokenToLexeme Else         = Just "else"
tokenToLexeme False_       = Just "false"
tokenToLexeme Fun          = Just "fun"
tokenToLexeme For          = Just "for"
tokenToLexeme If           = Just "if"
tokenToLexeme Nil          = Just "nil"
tokenToLexeme Or           = Just "or"
tokenToLexeme Print        = Just "print"
tokenToLexeme Return       = Just "return"
tokenToLexeme Super        = Just "super"
tokenToLexeme This         = Just "this"
tokenToLexeme True_        = Just "true"
tokenToLexeme Var          = Just "var"
tokenToLexeme While        = Just "while"
tokenToLexeme EOF          = Just ""
