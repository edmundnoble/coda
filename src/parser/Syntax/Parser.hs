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

import Text.Parsec.Prim

newtype TokenStream = TokenStream (Cat Token)

instance Monad m => Stream TokenStream m Token where
  uncons (TokenStream c) =
    pure $ over (_Just._2) TokenStream $ Control.Lens.uncons c

data Parsed = Parsed
  { parsedTopLevel :: OrErrs TopLevel TopLevelErr
  , parsedDo       :: OrErrs Do DoErr
  , parsedLet      :: OrErrs Let LetErr
  , parsedWhere    :: OrErrs Where WhereErr
  }

-- type TokParser = ParsecT

-- runParser :: Parser

type OrErrs a e = (Maybe a, Cat e)

data Tag a where
  TagTopLevel  :: Tag (OrErrs TopLevel TopLevelErr)
  TagDo        :: Tag (OrErrs Do DoErr)
  TagLet       :: Tag (OrErrs Let LetErr)
  TagWhere     :: Tag (OrErrs Where WhereErr)

data CAFDeclInfo = CAFDeclInfo
data DataDeclInfo = DataDeclInfo

data TyName = TyName
data TermName = TermName

data Expr = ExprDo Do | ExprLet Let | ExprWhere Where
data ExprErr

data Do = Do [DoStatement]
data DoErr

data Let
data LetErr

data Where
data WhereErr

data Purity = Pure | Impure

data DoStatement = DoStatement (Maybe TermName) Purity Expr

data TopLevel
  = CAFDecl TermName CAFDeclInfo Expr
  | DataDecl DataDeclInfo
  | TypeAlias TyName TyName
data TopLevelErr

parseExpr :: Dyck -> OrErrs Expr ExprErr
parseExpr _ = (Nothing, def)

parseTopLevel :: Dyck -> OrErrs TopLevel TopLevelErr
parseTopLevel _ = (Nothing, def)

parseDo :: Dyck -> OrErrs Do DoErr
parseDo _ = (Nothing, def)

parseLet :: Dyck -> OrErrs Let LetErr
parseLet _ = (Nothing, def)

parseWhere :: Dyck -> OrErrs Where WhereErr
parseWhere _ = (Nothing, def)

parse :: Dyck -> Parsed
parse d = Parsed (parseTopLevel d) (parseDo d) (parseLet d) (parseWhere d)

retrieve :: Parsed -> Tag a -> a
retrieve d TagTopLevel = parsedTopLevel d
retrieve d TagDo       = parsedDo d
retrieve d TagLet      = parsedLet d
retrieve d TagWhere    = parsedWhere d
