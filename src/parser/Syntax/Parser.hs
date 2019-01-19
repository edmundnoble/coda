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
import Relative.Cat(Cat)
import Rev(Rev(..))

import qualified Relative.Cat as Cat

import qualified Text.Parsec.Pos
import Text.Parsec.Prim hiding (parse)
import Text.Parsec.Error(ParseError)

newtype TokenStream = TokenStream (Cat Token)

-- how am I going to deal with dyck language stuff, do I need to?
instance Monad m => Stream TokenStream m Token where
  uncons (TokenStream c) =
    pure . over (_Just._2) TokenStream $ Control.Lens.uncons c

data Parsed = Parsed
  { parsedTopLevel :: Maybe TopLevel
  } deriving Show

type TokParser a = ParsecT TokenStream () Identity a

-- I'm pretty sure parsec's ParseError has completely broken source info, and
-- even if it weren't we have better info ourselves which we'll provide when
-- we're using a better parser "library" we don't care about sourcenames for
-- this reason.

-- also, is it accurate to call what we're making an "abstract syntax tree" when
-- we're only producing "levels" of the tree at a time? one odd consequence of
-- this would be that the `doParser` doesn't parse the `do` token! ask Ed to
-- elaborate on the syntax plans

-- looks like that last paragraph was complete bollocks

-- Ed has elaborated. It looks like for now it's nice to use this:
data P = P Dyck Parsed

instance Semigroup P where
  (P d _) <> (P d' _) = let d'' = d <> d' in P d'' (parse d'')

-- a systematic way to ignore comments?
-- a syntax for pragmas?

-- Dycks are NOT just lines, but candidates to become statements(!!!!!)

-- perhaps instantiate `ParsecT s u m` in a way which gives us parser errors
-- before swapping to a better parser?
-- seems impossible.
parseDyck :: TokParser a -> Dyck -> Maybe a
-- for now we eagerly fail if there are mismatched parens/braces/brackets.
parseDyck p (Dyck o s (Rev c) e _ _) | Cat.null o && Cat.null c
  = eitherToMaybe . runIdentity $ runParserT p () "" (TokenStream (s <> e))
  where
    eitherToMaybe = either (const Nothing) Just
parseDyck _ (Dyck _ _ _ _ _ _) = Nothing

data Tag a where
  TagTopLevel     :: Tag TopLevel
  TagDo           :: Tag Do
  TagLet          :: Tag Let
  TagWhere        :: Tag Where
  TagCase         :: Tag CaseBody
  TagClass        :: Tag ClassBody
  TagInstance     :: Tag InstanceBody

instance Show (Tag a) where
  show TagTopLevel     = "Tag TopLevel"
  show TagDo           = "Tag Do"
  show TagLet          = "Tag Let"
  show TagWhere        = "Tag Where"
  show TagCase         = "Tag Case"
  show TagClass        = "Tag Class"
  show TagInstance     = "Tag Instance"

data CAFDeclInfo = CAFDeclInfo
  deriving Show
data DataDeclInfo = DataDeclInfo
  deriving Show

data Pat

data CaseBody

data ClassBody

data InstanceBody

data ModuleHeader

tokenPos :: Text.Parsec.Pos.SourcePos -> Token -> Text.Parsec.Pos.SourcePos
tokenPos t = Text.Parsec.Pos.newPos "" 0 t

data TyName = TyName deriving Show
data TermName = TermName deriving Show

data Expr = ExprDo Do | ExprLet Let | ExprWhere Where
  deriving Show

data Do = Do [DoStatement] deriving Show

data Let = Let deriving Show

data Where = Where deriving Show

data Purity = Pure | Impure
  deriving Show

data DoStatement = DoStatement (Maybe TermName) Purity Expr
  deriving Show

data TopLevel
  = Defn TermName CAFDeclInfo Expr
  | Data DataDeclInfo
  | TypeAlias TyName TyName
  deriving Show

topLevelParser :: TokParser TopLevel
topLevelParser = pure undefined

exprParser :: TokParser Expr
exprParser = pure undefined

patParser ::  TokParser Pat
patParser = pure undefined

parse :: Dyck -> Parsed
parse d = Parsed (parseDyck topLevelParser d)

retrieve :: Parsed -> Tag a -> Maybe a
retrieve p TagTopLevel = parsedTopLevel p
retrieve _ tag = error $ "unimplemented parser for tag " ++ show tag

-- retrieve d TagTopLevel = parsedTopLevel d
-- retrieve d TagDo       = parsedDo d
-- retrieve d TagLet      = parsedLet d
-- retrieve d TagWhere    = parsedWhere d
