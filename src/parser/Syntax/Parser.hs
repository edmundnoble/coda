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
{-# language FunctionalDependencies #-}
{-# language ViewPatterns #-}

module Syntax.Parser where

import Control.Applicative((<|>), liftA2, some)
import Control.Lens
import Control.Monad(guard)
import Data.Bifunctor(first)
import Data.Functor(($>), void)
import Data.Text(Text)

import Syntax.Dyck
import Syntax.Name
import Syntax.Trifecta
import Syntax.Token
import Relative.Cat(Cat)
import Relative.Delta(Delta(..), HasDelta(..))
import Rev(Rev(..))

import qualified Relative.Cat as Cat

import qualified Text.Parsec.Pos
import Text.Parsec.Combinator(option, sepBy, sepBy1, sepEndBy1)
import Text.Parsec.Error(ParseError)
import Text.Parsec.Prim(ParsecT, Stream(..), (<?>), runParser, tokenPrim, try)

newtype TokenStream = TokenStream (Cat Token)

-- how am I going to deal with dyck language stuff, do I need to?
instance Monad m => Stream TokenStream m Token where
  uncons (TokenStream c) =
    pure . over (_Just._2) TokenStream $ Control.Lens.uncons c

class DyckParser m p | m -> p where
  enclosing :: p -> m a -> m a

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe = either (const Nothing) Just

data Parsed = Parsed
  { parsedTopLevel :: Maybe TopLevel
  , parsedDo :: Maybe Do
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
parseDyck :: TokParser a -> Dyck -> Either (Maybe ParseError) a
parseDyck p (Dyck o s (Rev c) e _ _) | Cat.null o && Cat.null c
  = first Just $ runParser p () "" (TokenStream (s <> e))
-- for now we eagerly fail if there are mismatched parens/braces/brackets.
parseDyck _ (Dyck _ _ _ _ _ _) = Left Nothing

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

-- for now, `Token` denotes what are called in Haskell "symbolic keywords"
rawTok ::
  Monad m =>
  Text -> ParsecT TokenStream u m ()
rawTok t = do
  _ <- satisfy $ \case
    TokenFlat (Token _ t') -> t == t'
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
  TokenFlat (TokenKeyword _ k') -> k == k'
  _ -> False

identP ::
  Monad m =>
  ParsecT TokenStream u m Name
identP = opt $ \case
  TokenFlat (TokenName _ n) -> Just n
  _ -> Nothing

satisfy ::
  Monad m =>
  (Token -> Bool) -> ParsecT TokenStream u m Token
satisfy f = opt (\t -> guard (f t) $> t)

data Expr
  = EDo Do
  | ELet Pat Expr Expr
  | EWhere Where
  | ELam Pat Expr
  | EIdent Name
  | EApp Expr Expr
  | ETuple [Expr]
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
  | TyAlias Name Name
  deriving Show

topLevelP :: TokParser TopLevel
topLevelP = undefined

-- this looks *very* broken.
inParens :: TokParser a -> TokParser a
inParens p = opt $ \case
  TokenNested Paren _ ts _ ->
    eitherToMaybe $ runParser p () "" (TokenStream ts)
  _ -> Nothing

exprP :: TokParser Expr
exprP
  = (EDo <$> doP) <|>
    lamP <|>
    letP <|>
    inParens (exprP <|> tupleP) <|>
    appOrIdentP
  where
  appOrIdentP = do
    i <- identP <?> "identifier"
    try (EApp (EIdent i) <$> exprP) <|> pure (EIdent i) <?> "function application"
  lamP = ELam <$>
    (rawTok "\\" *> patP) <*>
    (rawTok "->" *> exprP) <?> "lambda"
  letP = ELet <$>
    (keyword KLet *> patP) <*>
    (rawTok "=" *> exprP) <*>
    (keyword KIn *> exprP) <?> "let-binding"
  tupleP = ETuple <$> sepBy exprP (rawTok ",") <?> "tuple"

-- fixme
patP ::  TokParser Pat
patP = Pat <$> identP <?> "pattern"

doStmtP :: TokParser DoStatement
doStmtP
  = try (pur <|> impur) <*> exprP <|> (DoStatement Nothing Impure <$> exprP <?> "do-statement")
  where
  pur = flip DoStatement Pure <$>
    (keyword KLet *> (Just <$> patP) <* rawTok "=") <?> "do-let-binding"
  impur = flip DoStatement Impure <$>
    (Just <$> patP <* rawTok "<-") <?> "do-binding"

-- this doesn't work; we have no idea how to denote multiple
-- statements. We need a Cat Dyck or Free Cat Dyck or something.
doP :: TokParser Do
doP
  = keyword KDo *> (Do <$> (sepBy1 doStmtP (option () (void $ rawTok ";"))))
    <?> "do"

parse :: Dyck -> Parsed
parse = let
  parseTopLevel = parseDyck topLevelP
  parseDo = parseDyck doP in
  Parsed <$> (eitherToMaybe . parseTopLevel) <*> (eitherToMaybe . parseDo)

retrieve :: Parsed -> Tag a -> Maybe a
retrieve p TagTopLevel = parsedTopLevel p
-- retrieve d TagTopLevel = parsedTopLevel d
retrieve p TagDo       = parsedDo p
-- retrieve d TagLet      = parsedLet d
-- retrieve d TagWhere    = parsedWhere d
retrieve _ tag = error $ "unimplemented parser for tag " ++ show tag
