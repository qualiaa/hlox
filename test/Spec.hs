module Main (main) where

import Test.Tasty

import qualified LexerSpec

main :: IO ()
main = defaultMain $ testGroup "LoxTestSuite" [
      LexerSpec.allTests
    ]
