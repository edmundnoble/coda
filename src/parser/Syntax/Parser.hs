{-# language DeriveAnyClass #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language FlexibleContexts #-}
{-# language FlexibleInstances #-}
{-# language LambdaCase #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedStrings #-}
{-# language GADTs #-}
{-# language StandaloneDeriving #-}
{-# language TypeFamilies #-}
{-# language ViewPatterns #-}

module Syntax.Parser where

import Control.Applicative((<|>), some)
import Control.Lens
import Control.Monad(guard)
import Data.Default
import Data.Functor(($>))
import Data.Text(Text)

import Syntax.Dyck
import Syntax.Name
import Syntax.Token
import Relative.Cat(Cat)
import Relative.Delta(Delta(..), HasDelta(..))
import Rev(Rev(..))

import qualified Relative.Cat as Cat

import qualified Text.Parsec.Pos
import Text.Parsec.Combinator(optionMaybe)
import Text.Parsec.Prim(ParsecT, Stream(..), runParser, tokenPrim, try)

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

-- Turns out the above is kinda wrong, we can just store deltas in
-- Text.Parsec.Pos.SourcePos and pull them out later ourselves.

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
  = eitherToMaybe $ runParser p () "" (TokenStream (s <> e))
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

newtype Pat = Pat Name deriving Show

data CaseBody

data ClassBody

data InstanceBody

data ModuleHeader

-- throw out old position info, shove delta into columns slot
tokenPos ::
  Text.Parsec.Pos.SourcePos -> Token -> TokenStream -> Text.Parsec.Pos.SourcePos
tokenPos _ (delta -> Delta x) _
  = Text.Parsec.Pos.newPos "" 0 x

rawTok ::
  Monad m =>
  Text -> ParsecT TokenStream u m ()
rawTok t = do
  _ <- satisfy $ \case
    Token _ t' -> t == t'
    _ -> False
  return ()

opt ::
  Monad m =>
  (Token -> Maybe a) -> ParsecT TokenStream u m a
opt f = tokenPrim show tokenPos f

anyTok ::
  Monad m =>
  ParsecT TokenStream u m Token
anyTok = opt Just

keyword ::
  Monad m =>
  Keyword -> ParsecT TokenStream u m Token
keyword k = satisfy $ \case
  TokenKeyword _ k' -> k == k'
  _ -> False

identP ::
  Monad m =>
  ParsecT TokenStream u m Name
identP = opt $ \case
  TokenName _ n -> Just n
  _ -> Nothing

satisfy ::
  Monad m =>
  (Token -> Bool) -> ParsecT TokenStream u m Token
satisfy f = opt (\t -> guard (f t) $> t)

data Expr = EDo Do | ELet Let | EWhere Where | ELam Name Expr | EIdent Name
  deriving Show

data Do = Do [DoStatement] deriving Show

data Let = Let deriving Show

data Where = Where deriving Show

data Purity = Pure | Impure
  deriving Show

data DoStatement = DoStatement (Maybe Pat) Purity Expr
  deriving Show

data TopLevel
  = TySig Name Expr
  | Defn Name CAFDeclInfo Expr
  | Data DataDeclInfo
  -- invariant: LHS of a type alias must be unqualified
  | TyAlias Name Name
  deriving Show

topLevelP :: TokParser TopLevel
topLevelP = undefined

doStmtP :: TokParser DoStatement
doStmtP = pur <|> impur
  where
  pur = flip DoStatement Pure <$> (keyword KLet *> (Just <$> patP) <* rawTok "=") <*> exprP
  impur = flip DoStatement Impure <$> (optionMaybe patP <* rawTok "<-") <*> exprP

-- prime candidate for applicative parser composition
exprP :: TokParser Expr
exprP = lamP <|> letP <|> try appP <|> try (EIdent <$> identP)
  where
  appP = undefined -- EApp <$> exprP <*> exprP
  lamP = ELam <$> (rawTok "\\" *> identP) <*> (rawTok "->" *> exprP)
  letP = undefined

patP ::  TokParser Pat
patP = Pat <$> identP

doP :: TokParser Do
doP = keyword KDo *> (Do <$> some doStmtP)

parse :: Dyck -> Parsed
parse d = Parsed (parseDyck topLevelP d)

retrieve :: Parsed -> Tag a -> Maybe a
retrieve p TagTopLevel = parsedTopLevel p
retrieve _ tag = error $ "unimplemented parser for tag " ++ show tag

-- retrieve d TagTopLevel = parsedTopLevel d
-- retrieve d TagDo       = parsedDo d
-- retrieve d TagLet      = parsedLet d
-- retrieve d TagWhere    = parsedWhere d
