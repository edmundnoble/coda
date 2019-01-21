{-|
Copyright   : (c) 2018, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module ParserTest where

import Data.Char (isSpace)
import Data.Foldable
import Data.Text (Text)
import qualified Data.Text as Text
import Text.Parsec.Combinator(chainl1, eof)
import Text.Parsec.Error(ParseError)
import Text.Parsec.Prim(runParser)

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import FingerTree hiding (Position)
import Syntax.Prefix
import Syntax.Token
import Syntax.Dyck
import Syntax.Rope
import Relative.Class
import Relative.Delta
import Language.Server.Protocol (Position(..))
import qualified Syntax.Lexer as Lex
import Syntax.Layout
import qualified Syntax.Parser as Parse

parseExpr :: Text -> Either (Maybe ParseError) Parse.Expr
parseExpr = Parse.parseDyck (Parse.exprP <* eof) . Lex.lex

parseDoStmts :: Text -> Either (Maybe ParseError) [Parse.DoStatement]
parseDoStmts
  = traverse (Parse.parseDyck (Parse.doStmtP <* eof) . Lex.lex) . Text.lines

parse :: Text -> Parse.Parsed
parse = Parse.parse . Lex.lex
