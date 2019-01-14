{-# language DeriveAnyClass #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}

module Syntax.Parser where

import Control.Lens
import Data.Default

import Syntax.Dyck
import Syntax.Token
import Relative.Cat

import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Error(ParseError)

newtype TokenStream = TokenStream (Cat Token)

-- how am I going to deal with dyck language stuff, do I need to?
instance Monad m => Stream TokenStream m Token where
  uncons (TokenStream c) =
    pure . over (_Just._2) TokenStream $ Control.Lens.uncons c

data Parsed = Parsed
  { parsedTopLevel :: OrErrs TopLevel TopLevelErr
  , parsedDo       :: OrErrs Do DoErr
  , parsedLet      :: OrErrs Let LetErr
  , parsedWhere    :: OrErrs Where WhereErr
  }

type TokParser m a = ParsecT TokenStream () m a

-- I'm pretty sure parsec's ParseError has completely broken source info, and
-- even if it weren't we have better info ourselves which we'll provide when
-- we're using a better parser "library"
-- we don't care about sourcenames for this reason.

-- also, is it accurate to call what we're making an "abstract syntax tree" when
-- we're only producing "levels" of the tree at a time? one odd consequence of
-- this would be that the `doParser` doesn't parse the `do` token!
-- ask Ed to elaborate on the syntax plans

-- Ed has elaborated. It looks like for now it's nice to use this:
data P = P Dyck Parsed

instance Semigroup P where
  (P d _) <> (P d' _) = let d'' = d <> d' in P d'' (parse d'')

-- a systematic way to ignore comments?
-- a syntax for pragmas?

-- Dycks are NOT just lines, but candidates to become statements(!!!!!)
parseDyck :: Monad m => TokParser m a -> Dyck -> m (Either ParseError a)
parseDyck p (Dyck _ _ _ s _ _) = runParserT p () "" (TokenStream s)

type OrErrs a e = Either (Cat e) a

data Tag a e where
  TagTopLevel     :: Tag TopLevel TopLevelErr
  TagDo           :: Tag Do DoErr
  TagLet          :: Tag Let LetErr
  TagWhere        :: Tag Where WhereErr
  -- TagCase         :: Tag Case CaseErr
  -- TagClass        :: Tag Class ClassErr
  -- TagInstance     :: Tag Instance InstanceErr
  -- TagModuleHeader :: Tag ModuleHeader ModuleHeaderErr

data CAFDeclInfo = CAFDeclInfo
data DataDeclInfo = DataDeclInfo

data Pat

data TyName = TyName
data TermName = TermName

data Expr = ExprDo Do | ExprLet Let | ExprWhere Where
type ExprErr = ParseError

data Do = Do [DoStatement]
type DoErr = ParseError

data Let
type LetErr = ParseError

data Where
type WhereErr = ParseError

data Purity = Pure | Impure

data DoStatement = DoStatement (Maybe TermName) Purity Expr

data TopLevel
  = CAFDecl TermName CAFDeclInfo Expr
  | DataDecl DataDeclInfo
  | TypeAlias TyName TyName
data TopLevelErr

exprParser :: Monad m => TokParser m Expr
exprParser = pure undefined

patParser :: Monad m => TokParser m Pat
patParser = pure undefined

parseExpr :: Dyck -> OrErrs Expr ExprErr
parseExpr _ = undefined

parseTopLevel :: Dyck -> OrErrs TopLevel TopLevelErr
parseTopLevel _ = undefined

parseDo :: Dyck -> OrErrs Do DoErr
parseDo _ = undefined

parseLet :: Dyck -> OrErrs Let LetErr
parseLet _ = undefined

parseWhere :: Dyck -> OrErrs Where WhereErr
parseWhere _ = undefined

parse :: Dyck -> Parsed
parse d = Parsed (parseTopLevel d) (parseDo d) (parseLet d) (parseWhere d)

retrieve :: Parsed -> Tag a e -> Either e a
retrieve _ = undefined
-- retrieve d TagTopLevel = parsedTopLevel d
-- retrieve d TagDo       = parsedDo d
-- retrieve d TagLet      = parsedLet d
-- retrieve d TagWhere    = parsedWhere d
