{-# language DeriveDataTypeable #-}
{-# language DeriveGeneric #-}
{-# language FlexibleInstances #-}
{-# language FlexibleContexts #-}
{-# language MultiParamTypeClasses #-}
{-# language OverloadedLists #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language RankNTypes #-}
{-# language DataKinds #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017-2019
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Syntax.Token
  ( Token(..)
  , TokenFlat(..)
  , Pair(..)
  , LayoutMode(..)
  , Keyword(..)
  , keywords
  , startingKeywords
  , layoutKeywords
  , flat
  , nested
  , mismatch
  , unmatchedOpening
  , unmatchedClosing
  , lexicalError
  ) where

import Data.Data
import Data.Default
import Data.Reflection
import Data.Ix
import Data.Set as Set
import Data.Text (Text)
import GHC.Generics

import Relative.Cat
import Relative.Class
import Relative.Delta
import Relative.Located
import Syntax.Name

-- | these are keywords that are only valid at the start of a top level statement
startingKeywords :: Set String
startingKeywords =
  [ "class", "data", "default", "import", "infix", "infixl"
  , "infixr", "instance", "module", "newtype", "type"
  ]

-- | These are keywords that may occur anywhere in a source file
keywords :: Set String
keywords = ["as", "case", "deriving", "else" , "hiding", "if", "in", "qualified", "then" ]

-- | These are keywords that introduce layout
layoutKeywords :: Set String
layoutKeywords = ["do","let","of","where"]

data Keyword
  = KAs
  | KCase
  | KClass
  | KData
  | KDefault
  | KDeriving
  | KDo
  | KElse
  | KHiding
  | KIf
  | KImport
  | KIn
  | KInfix
  | KInfixl
  | KInfixr
  | KInstance
  | KLet
  | KModule
  | KNewtype
  | KOf
  | KQualified
  | KThen
  | KType
  | KWhere
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Data,Generic)

data TokenFlat
  = Token                 {-# unpack #-} !Delta {-# unpack #-} !Text -- as yet uninterpreted lexemes
  | TokenName             {-# unpack #-} !Delta !Name
  | TokenKeyword          {-# unpack #-} !Delta !Keyword
  | TokenInteger          {-# unpack #-} !Delta !Integer
  | TokenDouble           {-# unpack #-} !Delta {-# unpack #-} !Double
  | TokenString           {-# unpack #-} !Delta {-# unpack #-} !Text
  | TokenChar             {-# unpack #-} !Delta {-# unpack #-} !Char
  | TokenUnmatchedOpening {-# unpack #-} !(Located Pair)
  | TokenUnmatchedClosing {-# unpack #-} !(Located Pair)
  | TokenLexicalError     {-# unpack #-} !Delta String
  deriving (Eq,Ord,Show,Read)

data Token
  = TokenFlat     !TokenFlat
  | TokenNested   !Pair {-# unpack #-} !Delta !(Cat Token) {-# unpack #-} !Delta
  | TokenMismatch {-# unpack #-} !(Located Pair) {-# unpack #-} !(Located Pair) !(Cat Token)
  deriving (Eq,Ord,Show,Read)

flat :: TokenFlat -> Token
flat = TokenFlat

nested :: Pair -> Delta -> Cat Token -> Delta -> Token
nested = TokenNested

mismatch :: Located Pair -> Located Pair -> Cat Token -> Token
mismatch = TokenMismatch

unmatchedOpening :: Located Pair -> TokenFlat
unmatchedOpening = TokenUnmatchedOpening

unmatchedClosing :: Located Pair -> TokenFlat
unmatchedClosing = TokenUnmatchedClosing

lexicalError :: Delta -> String -> TokenFlat
lexicalError = TokenLexicalError

instance Relative TokenFlat where
  rel 0 xs = xs
  rel d0 xs0 = go d0 xs0 where
    go d (Token d' t) = Token (d+d') t
    go d (TokenName d' n) = TokenName (d+d') n
    go d (TokenKeyword d' k) = TokenKeyword (d+d') k
    go d (TokenInteger d' i) = TokenInteger (d+d') i
    go d (TokenDouble d' f) = TokenDouble (d+d') f
    go d (TokenString d' l) = TokenString (d+d') l
    go d (TokenChar d' l) = TokenChar (d+d') l
    go d (TokenUnmatchedOpening dp) = TokenUnmatchedOpening (rel d dp)
    go d (TokenUnmatchedClosing dp) = TokenUnmatchedClosing (rel d dp)
    go d (TokenLexicalError d' s) = TokenLexicalError (d+d') s

instance Relative Token where
  rel 0 xs = xs
  rel d (TokenFlat t) = TokenFlat $ rel d t
  rel d (TokenNested p dp ts dq) = TokenNested p (rel d dp) (rel d ts) (rel d dq)
  rel d (TokenMismatch dp dq ts) = TokenMismatch (rel d dp) (rel d dq) (rel d ts)

-- I don't see this having any legitimate uses after we start working with spans
-- directly. -- Ed 2
instance HasDelta TokenFlat where
  delta (Token d _) = d
  delta (TokenName d _) = d
  delta (TokenKeyword d _) = d
  delta (TokenInteger d _) = d
  delta (TokenDouble d _) = d
  delta (TokenString d _) = d
  delta (TokenChar d _) = d
  delta (TokenUnmatchedOpening (Located d _)) = d
  delta (TokenUnmatchedClosing (Located d _)) = d
  delta (TokenLexicalError d _) = d

instance HasDelta Token where
  delta (TokenFlat t) = delta t
  delta (TokenNested _ d _ _) = d
  delta (TokenMismatch (Located d _) _ _) = d

data Pair = Brace | Bracket | Paren
  deriving (Eq,Ord,Show,Read,Ix,Enum,Bounded,Generic)

data LayoutMode = LNone | LDo | LLet | LOf | LWhere
  deriving (Eq,Ord,Show,Read)

instance Default LayoutMode where
  def = LNone
