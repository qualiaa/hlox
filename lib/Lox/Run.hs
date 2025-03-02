{-# LANGUAGE LambdaCase #-}
module Lox.Run where

import Control.Monad (forever, foldM_)
import Control.Monad.IO.Class (liftIO)
import Control.Exception (SomeException(..), Exception, catch, try, throwIO)
import Data.Typeable (Typeable, cast)
import Data.Default (def)
import Data.List (sortBy, foldl')
import Data.Ord (comparing)
import System.Console.ANSI.Codes(setSGRCode)
import System.Console.ANSI.Types ( SGR(SetConsoleIntensity, SetUnderlining, SetRGBColor, SetColor))
import System.IO (hPrint, hPutStr, hPutStrLn, readFile', stderr)
import System.IO.Error (isEOFError)
import Prelude hiding (lex)

import qualified Data.Colour.Names as Col
import qualified System.Console.ANSI.Types as Console ( Color(..)
                                                      , ConsoleLayer(..)
                                                      , ColorIntensity(Vivid, Dull)
                                                      , ConsoleIntensity(BoldIntensity)
                                                      )


import Lox.Config
import Lox.Lexer (lex, LexError(..), LexState(..), startLoc, endLoc)
import Lox.Loc(Loc(..), inc, adjacent, steps)


data LoxException = LexicalException | ParseException deriving (Show, Typeable)
instance Exception LoxException

runPrompt :: IO ()
runPrompt = do
  welcomeMessage
  repl `catch` \(e :: IOError) -> if isEOFError e then hPutStrLn stderr "See ya!"
                                  else throwIO e

  where repl :: IO ()
        repl = forever (getLine >>= try . run :: IO (Either LoxException ()))

runFile :: String -> IO ()
runFile filePath = do
  hPutStrLn stderr  ("Received " ++ filePath)
  readFile' filePath >>= run

run :: String -> IO ()
run code = do
  case lex () code of
    Left errors -> printErrors code errors >> throwIO LexicalException
    Right tokens -> mapM_ print tokens


printErrors :: String -> [LexError] -> IO ()
printErrors _ [] = hPutStrLn stderr "Unknown error occurred!"
printErrors s errs =
  let errs' = merge $ sortByLoc errs
      errLines = getLines 0 def (map (startLoc . fst) errs') (lines s)
  in mapM_ (uncurry printError) $ zip errs' errLines

  where sortByLoc :: [LexError] -> [LexError]
        sortByLoc = sortBy (comparing startLoc)

        -- A series of adjacent UnexpectedCharacters can be merged into one error
        merge :: [LexError] -> [(LexError, String)]
        merge [] = []
        merge (x:xs) = reverse $ foldl' joinAdjacent [(x, "")] xs

        joinAdjacent a@((aErr@(UnexpectedCharacter loc0 _), aS):aErrs) err1@(UnexpectedCharacter loc1 c1) =
          if inc (length aS) loc0  `adjacent` loc1 then (aErr, aS ++ [c1]):aErrs else (err1, ""):a
        joinAdjacent a err = (err, ""):a

        getLines :: Int -> Loc -> [Loc] -> [String] -> [(Int, String, Int)]
        getLines _ _ [] _                     = []
        getLines _ _ _ []                     = error "Ran out of lines"
        getLines lineNo pos (loc:locs) (l:ls) =
          let lineEnd = inc (length l) pos
          in if loc > lineEnd then getLines (lineNo + 1) (inc 1 lineEnd) (loc:locs) ls
          else (lineNo, l, steps pos loc) : getLines lineNo pos locs (l:ls)

        printError :: (LexError, String) -> (Int, String, Int) -> IO ()
        printError (err, extraChars) (lineNo, line, startChar) = do

          let errLen = length extraChars + steps (startLoc err) (endLoc err)
              errLen' = min errLen $ length line - startChar

          hPutStrLn stderr ""
          hPutStr   stderr $ style [bold] (show lineNo ++  ":" ++ show startChar ++ colon)
          hPutStrLn stderr $ prettyErr err extraChars
          hPutStrLn stderr $ "    " ++ style badCodeLineStyle line
          hPutStr   stderr $ replicate (startChar + 4) ' '
          hPutStrLn stderr $ errMarker errLen'

        errMarker errLen = style errorMarkerStyle $ '^' : replicate (errLen-1) '~'

        prettyErr (LexError _ s) _             = mconcat [ style errorStyle "Lexical Error"
                                                         , style [bold] (colon ++ s)
                                                         ]
        prettyErr (UnexpectedCharacter _ c) "" = mconcat [ style errorStyle "Unexpected character"
                                                         , style [bold] (colon ++ quote [c])
                                                         ]
        prettyErr (UnexpectedCharacter _ c) s  = mconcat [ style errorStyle "Unexpected characters"
                                                         , style [bold] (colon ++ quote (c:s))
                                                         ]
        prettyErr (UnterminatedString _ _) _   = style errorStyle "Unterminated string"

style :: [SGR] -> String -> String
style sgr s = setSGRCode sgr ++ s ++ setSGRCode []

bold = SetConsoleIntensity Console.BoldIntensity

errorStyle = [ bold , SetColor Console.Foreground Console.Vivid Console.Red]
badCodeLineStyle = [ SetColor Console.Foreground Console.Dull Console.Cyan ]
errorMarkerStyle = [ SetColor Console.Foreground Console.Vivid Console.White ]

colon :: String
colon = ": "

quote :: String -> String
quote s = '‘' : s ++ "’"

welcomeMessage :: IO ()
welcomeMessage = do
  putStr $ setSGRCode [bold]
  mapM_ (\(c, (bg, fg)) -> putStr $ setSGRCode (sgr bg fg) ++ [c]) $ zip msgString colorCycle
  putStrLn $ setSGRCode []


  where sgr bg fg = [ SetRGBColor Console.Foreground fg
                    , SetRGBColor Console.Background bg]


        colorCycle = cycle [ (Col.red,    Col.green)
                           , (Col.orange, Col.blue)
                           , (Col.yellow, Col.indigo)
                           , (Col.green,  Col.red)
                           , (Col.blue,   Col.orange)
                           , (Col.indigo, Col.yellow)
                           ]
        msgString = "Welcome to the Lox interpreter!"
