{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module LayoutTest where

import Data.Char (isSpace)
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import FingerTree hiding (Position)
import Syntax.Prefix
import Syntax.Token
import Syntax.Dyck
import Syntax.Rope
import Relative.Class
import Relative.Delta
import Language.Server.Protocol (Position(..))
import qualified Syntax.Lexer as Lex
import Layout

ptxt :: Int -> Text -> Layout
ptxt n = dyckLayout 0 (Prefix . Text.pack . replicate n $ ' ') . Lex.lex

txt :: Text -> Layout
txt = dyckLayout 0 (Prefix "") . Lex.lex

exampleA :: Text
exampleA =
  "do \n\
  \   x\n\
  \   y\n\
  \"

exampleB :: Text
exampleB =
  "foo \n\
  \   x\n\
  \   y\n\
  \"

exampleC :: Text
exampleC =
  "do \n\
  \   x\n\
  \ \t y\n\
  \"

exampleD :: Text
exampleD =
  "foo \n\
  \   x\n\
  \ \t y\n\
  \"

example0_0 :: Text
example0_0 =
  "let \n\
  \{  x = 1\n\
  \ \t y = 2\n\
  \   z = 3\n\
  \}"

example0_1a :: Text
example0_1a =
  "let \n"
example0_1b :: Text
example0_1b =
  "{  x = 1\n\
  \ \t y = 2\n\
  \   z = 3\n\
  \}"

example0_2a :: Text
example0_2a =
  "let \n\
  \  x = 1\n"
example0_2b :: Text
example0_2b =
  "\t y = 2\n\
  \  z = 3\n\
  \"

example0_3a :: Text
example0_3a =
  "let \n\
  \  x = 1\n\
  \t y = 2\n"
example0_3b :: Text
example0_3b =
  "  z = 3\n\
  \"

example0_4a :: Text
example0_4a =
  "let \n\
  \  x = 1\n\
  \t y = 2\n\
  \  z = 3\n"
example0_4b :: Text
example0_4b =
  ""

example0 :: Text
example0 =
  "let \n\
  \  x = 1\n\
  \\t y = 2\n\
  \  z = 3\n\
  \"

example1 :: Text
example1 =
  "let \n\
  \{  x = 1\n\
  \ \t y = 2\n\
  \   z = 3\n\
  \}"

example2 :: Text
example2 =
  "let \n\
  \{ x = let \n\
  \      { a = 1\n\
  \        b = 1\n\
  \      }\n\
  \  y = let \n\
  \      { c = 3\n\
  \        d = 4\n\
  \      }\n\
  \  z = let \n\
  \      { e = 5\n\
  \        f = 6\n\
  \      }\n\
  \}"

linesToLayouts :: Delta -> [Text] -> (Delta, [Layout])
linesToLayouts d0 ls =
  let
    f (d, ls) t =
      ( d <> fromIntegral (Text.length t)
      , ls <> pure (dyckLayout d (Prefix . Text.takeWhile isSpace $ t) (rel d (Lex.lex t)))
      )
  in
    foldl' f (d0, mempty) ls

textToLayoutPerLine :: Text -> [Layout]
textToLayoutPerLine =
  snd . linesToLayouts 0 . Text.lines

textToLayouts :: Text -> [Layout]
textToLayouts t =
  let
    ts = Text.lines t
    f :: Int -> Layout
    f i = (\(x, y) -> g x y) $ splitAt i ts
    g :: [Text] -> [Text] -> Layout
    g x y =
      let
        (d, ls1) = linesToLayouts 0 x
        (_, ls2) = linesToLayouts d y
      in
        fold ls1 <> fold ls2
  in
    fmap f [0..length ts - 1]

textsToLayout :: Text -> Text -> Layout
textsToLayout t1 t2 =
  let
    (d, ls1) = linesToLayouts 0 (Text.lines t1)
    (_, ls2) = linesToLayouts d (Text.lines t2)
  in
    foldl (<>) mempty ls1 <> foldl (<>) mempty ls2

allEq :: Eq a => [a] -> Bool
allEq xs =
  and $ zipWith (==) xs (tail xs)

test_layout :: TestTree
test_layout = testGroup "layout"
  [
    -- testCase "ed" $ E 0 <> txt "a" @=? txt "a"
  -- , testCase "de" $ txt "a" <> E 0 @=? txt "a"
  -- , testCase "ee" $ E 1 <> E 2 @=? E 3

    testCase "A" $ E 0 @=? textsToLayout exampleA ""
  , testCase "B" $ E 0 @=? textsToLayout exampleB ""
  , testCase "C" $ E 0 @=? textsToLayout exampleC ""
  , testCase "D" $ E 0 @=? textsToLayout exampleD ""

  -- just taking a peek at what is going on with example1
  -- , testCase "0" $ E 0 @=? textsToLayout example0_0 ""
  -- , testCase "1" $ textsToLayout example0_0 "" @=? textsToLayout example0_1a example0_1b
  -- , testCase "2" $ textsToLayout example0_0 "" @=? textsToLayout example0_2a example0_2b
  -- , testCase "3" $ textsToLayout example0_0 "" @=? textsToLayout example0_3a example0_3b
  -- , testCase "4" $ textsToLayout example0_0 "" @=? textsToLayout example0_4a example0_4b

  -- , testCase "example1" $ True @=? allEq (textToLayouts example1)
  -- , testCase "example2" $ True @=? allEq (textToLayouts example2)
  ]
