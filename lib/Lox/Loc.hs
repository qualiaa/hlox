module Lox.Loc where

import Data.Default (Default, def)

data Loc = Loc { codeLine :: !Int
               , codeCol  :: !Int
               } deriving Show

instance Default Loc where
  def = Loc 0 0

newLine :: Loc -> Loc
newLine (Loc { codeLine = l }) = Loc {codeLine=l+1, codeCol=0}

nextLine :: Loc -> Loc
nextLine loc@( Loc {codeLine = l} ) = loc { codeLine = l+1 }

nextCol :: Loc -> Loc
nextCol loc@( Loc {codeCol = c} ) = loc { codeCol = c+1 }

adjacent :: Loc -> Loc -> Bool
adjacent (Loc l0 c0) (Loc l1 c1) = let diff = c1 - c0 in l0 == l1 && diff == -1 || diff == 1
