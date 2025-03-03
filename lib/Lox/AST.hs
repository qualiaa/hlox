module Lox.AST where

import Lox.Token (Token)

data Literal = LoxStr !String | LoxNum !Double | LoxBool !Bool | LoxNil deriving Show

data Expr = Literal LoxVal
          | Unary Token Expr
          | Binary Expr Token Expr
          | Grouping Expr
     deriving Show
