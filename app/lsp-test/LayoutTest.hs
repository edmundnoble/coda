{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
module LayoutTest where

import Data.Text (Text)
import qualified Data.Text as Text

import Test.Tasty
import Test.Tasty.HUnit

import Syntax.Prefix
import Syntax.Token
import Syntax.Dyck
import Layout

ltxt :: Text -> Text -> Layout
ltxt p t = dyckLayout ((+ 1) . fromIntegral $ Text.length p) (Prefix p) (layoutToken mempty LDo (Token 1 t))

ptxt :: Text -> Text -> Layout
ptxt p t = dyckLayout ((+ 1) . fromIntegral $ Text.length p) (Prefix p) (token mempty (Token 1 t))

txt :: Text -> Layout
txt t = dyckLayout 1 (Prefix "") (token mempty (Token 1 t))

test_layout :: TestTree
test_layout = testGroup "layout"
  [ testCase "ed" $ E 0 <> txt "a" @=? txt "a"
  , testCase "de" $ txt "a" <> E 0 @=? txt "a"
  , testCase "ee" $ E 1 <> E 2 @=? E 3
  -- just playing around to get a feel for things
  , testCase "dd" $ txt "a" <> txt "b" @=? E 3
  , testCase "pdpd" $ ptxt " " "a" <> ptxt "  " "b" @=? E 3
  , testCase "ldld" $ ltxt " " "a" <> ltxt "  " "b" @=? E 3
  ]
