module LexerSpec (allTests) where

import Prelude hiding (lex)
import Data.Char (isAlphaNum, isPrint, isSpace, ord, chr)
import Data.Default (def)
import Data.Foldable (toList)
import Data.List.NonEmpty (NonEmpty(..))
import Data.Maybe (fromJust)
import qualified Data.List.NonEmpty as NE
import Data.Either (fromRight)

import Test.Tasty
import Test.Hspec.Expectations
import Test.Tasty.Falsify
import Test.Falsify.Predicate ((.$))
import qualified Test.Falsify.Generator as FG
import qualified Test.Falsify.Range as FR
import qualified Test.Falsify.Predicate as FP

import Lox.Lexer (runLex, evalLex, execLex, lex, LexError(..))
import qualified Lox.Token as Tok
import Utils


allTests = testGroup "Lexer" $ do
   unitTests ++ falsifyTests

unitTests = [

    specify "empty string gives eof" $

      lex def "" `shouldSatisfy` expectToken Tok.EOF

  , specify "// is comment" $

      lex def "//" `shouldSatisfy` expectToken Tok.EOF

  , specify "/ / is not comment" $

      lex def "/ /" `shouldSatisfy` expectTokens (Tok.Slash :| [Tok.Slash, Tok.EOF])

  , specify ".0 is not a number" $

      lex def ".0" `shouldSatisfy` expectTokens (Tok.Dot :| [Tok.Number, Tok.EOF])

  , specify "-0 is not a number" $

      lex def "-0" `shouldSatisfy` expectTokens (Tok.Minus :| [Tok.Number, Tok.EOF])

  , specify "unicode identifiers allowed" $

      lex def "θ" `shouldSatisfy` expectTokens (Tok.Identifier :| [Tok.EOF])
  ] ++ [
    specify ("lex " ++ show token) $

      lex def lexeme `shouldSatisfy` expectTokens (token :| [Tok.EOF])

    | (token, lexeme) <- simpleTokens
  ]


falsifyTests :: [TestTree]
falsifyTests = [
    testPropertyWith (def {overrideNumTests=Just 10000}) "comments ignored" prop_commentsIgnored
  , testPropertyWith (def {overrideNumTests=Just 10000}) "comments don't affect lex" prop_commentsDontAffectLex
  , testPropertyWith (def {overrideNumTests=Just 10000}) "lexemes don't collide" prop_lexemesDontCollide
  ] ++ [
    testPropertyWith (def {overrideNumTests=Just 10000}) ("lex valid " ++ show t)  prop
    | (t, prop) <- props_lexemesLex]

expectToken :: Tok.TokenType -> Either e [Tok.Token] -> Bool
expectToken t = expectTokens (NE.singleton t)

expectTokens :: NonEmpty Tok.TokenType -> Either e [Tok.Token] -> Bool
expectTokens _ (Left _) = False
expectTokens _ (Right []) = False
expectTokens expected  (Right tokens) = all (uncurry (==)) pairs
  where pairs = zip (toList expected) (Tok.tokenType <$> tokens)

genChar :: Gen Char
genChar = FG.frequency [ (2, FG.inRange (FR.enum (chr 0, chr 127)))
                       , (1, FG.inRange (FR.enum (chr 128, maxBound)))
                       ]

prop_commentsIgnored :: Property ()
prop_commentsIgnored = do
  comment <- fmap (("//" ++) . filter isPrint) <$> gen $ FG.list (FR.between (0, 100)) genChar

  let lex' = FP.fn                ("lexed", lex def)
      isEOF = FP.satisfies        ("onlyEOF", expectToken Tok.EOF)

  assert $ isEOF `FP.dot` lex' .$ ("string",  comment)


prop_commentsDontAffectLex = do
  comment <-  fmap (("//" ++) . filter isPrint) <$> gen $ FG.list (FR.between (0, 20)) genChar

  let realLexemes = unwords <$> (FG.list (FR.between (1, 10)) genToken >>= mapM genLexemeFor)
      gibberish = FG.list (FR.between (1, 50)) genChar

  string <- gen $ FG.choose realLexemes gibberish

  let string' = comment ++ "\n" ++ string
      -- Tok.Token and LexError don't implement Eq
      lex' = FP.fn                ("lexed", ignoreLoc . lex def)

  assert $ FP.eq `FP.on` lex'
    .$ ("string", string)
    .$ ("string with comment", string')

  -- Should probably use lens for this...
  where ignoreLoc (Left es) = Left $ map go es
           where go (LexError _ s) = LexError def s
                 go (UnexpectedCharacter _ c) = UnexpectedCharacter def c
                 go (UnterminatedString _ _) = UnterminatedString def def
        ignoreLoc (Right ts) = Right $ map (\t -> t {Tok.tokenLoc = def}) ts


props_lexemesLex :: [(Tok.TokenType, Property ())]
props_lexemesLex = map go [ (Tok.Identifier, genIdentifierLexeme)
                          , (Tok.String_, genStringLexeme)
                          , (Tok.Number, genNumberLexeme)
                          ]
  where go (t, g) = (t, go' t g)
        go' t g = do
          lexeme <- gen $ g

          assert $
            FP.satisfies ("hasToken", expectTokens $ t :| [Tok.EOF])
            `FP.dot`
            FP.fn        ("lexed", lex def)
            .$           ("lexeme", lexeme)

prop_lexemesDontCollide ::Property ()
prop_lexemesDontCollide = do
  tokens <- gen $ (:|) <$> genToken <*> FG.list (FR.between (0, 9)) genToken
  lexemes <- gen $ mapM genLexemeFor tokens

  assert $ FP.satisfies ("hasToken", expectTokens $ tokens <> NE.singleton Tok.EOF)
           `FP.dot`
           FP.fn ("lexed",  lex def)
           .$    ("input from " ++ show tokens , unwords . toList $ lexemes)


genToken :: Gen Tok.TokenType
genToken = FG.inRange $ FR.enum (minBound, (pred maxBound)) -- maxBound is EOF

genIdentifierLexeme :: Gen String
genIdentifierLexeme = do
  firstChar <- FG.choose (FG.inRange (FR.enum ('A', 'Z')))
                         (FG.inRange (FR.enum ('a', 'z')))

  rest <- filter isAlphaNum <$> FG.list (FR.between (0, 100)) genChar

  let identifier = firstChar : rest

  case Tok.keyword identifier of
    Just _ -> genIdentifierLexeme
    Nothing -> return identifier

genStringLexeme :: Gen String
genStringLexeme = do
  str <- filter (\c -> c /= '"' && (isPrint c || isSpace c)) <$> FG.list (FR.between (0, 100)) genChar
  return $ '"':str ++ ['"']

genNumberLexeme :: Gen String
genNumberLexeme = do
  let digit = FG.inRange (FR.enum ('0', '9'))
  first <- digit
  last <- digit
  digits <- FG.list (FR.between (0, 15)) digit

  digits' <- FG.shuffle $ '.':digits

  FG.choose (return $ first:digits) (return $ (first:digits') ++ [last])

genLexemeFor :: Tok.TokenType -> Gen String
genLexemeFor Tok.Identifier = genIdentifierLexeme
genLexemeFor Tok.String_    = genStringLexeme
genLexemeFor Tok.Number     = genNumberLexeme
genLexemeFor t              = return $ fromJust $ Tok.tokenToLexeme t


simpleTokens :: [(Tok.TokenType, String)]
simpleTokens =
  let toks = filter (`notElem` [Tok.Identifier, Tok.String_, Tok.Number]) [minBound .. pred maxBound]
  in map (\t -> (t, fromJust $ Tok.tokenToLexeme t)) toks
