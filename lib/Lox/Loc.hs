module Lox.Loc where

data Loc = Loc { codeLine :: Int
               , codeCol :: Int
               } deriving Show

instance Semigroup Loc where
  (Loc l0 c0) <> (Loc l1 c1) = Loc (l0+l1) (c0+c1)

instance Monoid Loc where
  mempty = Loc 0 0
  mappend = (<>)
