module Utils where

import Test.Tasty.HUnit
import Data.Functor ((<&>))

-- Redefining hspec's synonyms for hunit primitives

it = testCase
specify = testCase
