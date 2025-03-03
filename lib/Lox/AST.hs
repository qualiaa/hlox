module Lox.AST where

import Lox.Token (Token)

data LoxVal = LoxStr !String | LoxNum !Double | LoxBool !Bool | LoxNil deriving (Eq, Show)

data Expr = Literal LoxVal
          | Unary Token Expr
          | Binary Expr Token Expr
          | Grouping Expr
     deriving Show
