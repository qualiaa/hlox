module Lox.Loc
  ( Loc
  , adjacent
  , inc
  ) where

import Data.Default (Default, def)

newtype Loc = Loc { unLoc :: Int } deriving (Enum, Show, Eq, Ord)

instance Default Loc where
  def = Loc 0

inc :: Int -> Loc -> Loc
inc n (Loc c) = Loc $ n + c

adjacent :: Loc -> Loc -> Bool
adjacent (Loc n) (Loc m) = abs (m - n) <= 1
