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

example1 :: Text
example1 =
  "let {\n\
  \\t x = 1\n\
  \  y = 2\n\
  \}"

example2 :: Text
example2 =
  "let {\n\
  \  x = let {\n\
  \        a = 1\n\
  \        b = 1\n\
  \      }\n\
  \  y = let {\n\
  \      c = 3\n\
  \      d = 4\n\
  \    }\n\
  \  z = let {\n\
  \        e = 5\n\
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

allEq :: Eq a => [a] -> Bool
allEq xs =
  and $ zipWith (==) xs (tail xs)

test_layout :: TestTree
test_layout = testGroup "layout"
  [ testCase "ed" $ E 0 <> txt "a" @=? txt "a"
  , testCase "de" $ txt "a" <> E 0 @=? txt "a"
  , testCase "ee" $ E 1 <> E 2 @=? E 3


  -- just taking a peek at what is going on with example1
  , testCase "blah" $ [] @=? textToLayouts example1

  , testCase "example1" $ True @=? allEq (textToLayouts example1)
  , testCase "example2" $ True @=? allEq (textToLayouts example2)

  , testCase "lexme" $ Lex.lex "do { 1 }" @=? token mempty (Token 2 "Hi")
  , testCase "lexme" $
      let
        p1 = "do { "
        l1 = Text.length p1
        t1 = Lex.lex p1
        p2 = "1 }"
        t2 = rel (fromIntegral l1) $ Lex.lex p2
      in
        t1 <> t2 @=? token mempty (Token 2 "Hi")

  -- , testCase "lexme" $ Lex.lex "do" @=? token mempty (Token 2 "Hi")
  --   -- ^ Dyck [] [] (Rev {runRev = []}) [TokenKeyword 0 KDo] LDo []

  -- , testCase "lexme" $ Lex.lex "do {" @=? token mempty (Token 2 "Hi")
  --   -- ^ Dyck [] [TokenKeyword 0 KDo] (Rev {runRev = [Opening (Located 3 Brace) []]}) [] LNone []

  -- , testCase "lexme" $ Lex.lex "do { 1" @=? token mempty (Token 2 "Hi")
  --   -- ^ Dyck [] [TokenKeyword 0 KDo] (Rev {runRev = [Opening (Located 3 Brace) []]}) [TokenInteger 5 1] LNone []

  -- , testCase "lexme" $ Lex.lex "do { 1 }" @=? token mempty (Token 2 "Hi")
  --   -- ^ Dyck [] [TokenKeyword 0 KDo] (Rev {runRev = []}) [TokenNested (Located 3 Brace) [TokenInteger 5 1]] LNone []

  -- , testCase "lexme" $ Lex.lex "do { 1 } do" @=? token mempty (Token 2 "Hi")
  --   -- ^ Dyck [] [TokenKeyword 0 KDo] (Rev {runRev = []}) [TokenNested (Located 3 Brace) [TokenInteger 5 1],TokenKeyword 9 KDo] LDo []

  -- , testCase "lexme" $ Lex.lex "do { 1 } do {" @=? token mempty (Token 2 "Hi")
  --   -- ^ Dyck [] [TokenKeyword 0 KDo,TokenNested (Located 3 Brace) [TokenInteger 5 1],TokenKeyword 9 KDo] (Rev {runRev = [Opening (Located 12 Brace) []]}) [] LNone []

  -- , testCase "lexme" $ Lex.lex "do { 1 } do { 2" @=? token mempty (Token 2 "Hi")
  --   -- ^ Dyck [] [TokenKeyword 0 KDo,TokenNested (Located 3 Brace) [TokenInteger 5 1],TokenKeyword 9 KDo] (Rev {runRev = [Opening (Located 12 Brace) []]}) [TokenInteger 14 2] LNone []

  -- , testCase "lexme" $ Lex.lex "do { 1 } do { 2 }" @=? token mempty (Token 2 "Hi")
  --   -- ^ Dyck [] [TokenKeyword 0 KDo,TokenNested (Located 3 Brace) [TokenInteger 5 1],TokenKeyword 9 KDo] (Rev {runRev = []}) [TokenNested (Located 12 Brace) [TokenInteger 14 2]] LNone []
  ]
