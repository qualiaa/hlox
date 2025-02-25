module Lox.Loc
  ( Loc
  , adjacent
  , inc
  , steps
  ) where

import Data.Default (Default, def)

newtype Loc = Loc { unLoc :: Int } deriving (Enum, Show, Eq, Ord)

instance Default Loc where
  def = Loc 0

inc :: Int -> Loc -> Loc
inc n (Loc c) = Loc $ n + c

steps :: Loc -> Loc -> Int
steps (Loc n) (Loc m) = m - n

adjacent :: Loc -> Loc -> Bool
adjacent a = (<= 1) . abs . steps a
