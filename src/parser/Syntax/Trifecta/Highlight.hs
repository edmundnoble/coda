{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

#ifndef MIN_VERSION_lens
#define MIN_VERSION_lens(x,y,z) 1
#endif
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
----------------------------------------------------------------------------
module Syntax.Trifecta.Highlight
  ( Highlight
  , HighlightedRope(HighlightedRope)
  , HasHighlightedRope(..)
  , withHighlight
  , HighlightDoc(HighlightDoc)
  , HasHighlightDoc(..)
  , doc
  ) where

import Control.Lens
#if MIN_VERSION_lens(4,13,0) && __GLASGOW_HASKELL__ >= 710
  hiding (Empty)
#endif
import Data.Foldable as F
import Data.Int (Int64)
import Data.List (sort)
import Data.Semigroup
import Data.Semigroup.Union
import Prelude hiding (head)
import Text.Blaze
import Text.Blaze.Html5 hiding (a,b,i)
import qualified Text.Blaze.Html5 as Html5
import Text.Blaze.Html5.Attributes hiding (title,id)
import Text.Blaze.Internal (MarkupM(Empty, Leaf))
import Text.Parser.Token.Highlight
import Data.Text.PrettyPrint.Doc
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.ByteString.Lazy.UTF8 as LazyUTF8

import Syntax.Trifecta.Util.IntervalMap as IM
import Syntax.Trifecta.Delta
import Syntax.Trifecta.Rope

-- | Convert a 'Highlight' into a coloration on a 'Doc'.
withHighlight :: Highlight -> Doc -> Doc
withHighlight Comment                     = blue
withHighlight ReservedIdentifier          = magenta
withHighlight ReservedConstructor         = magenta
withHighlight EscapeCode                  = magenta
withHighlight Operator                    = yellow
withHighlight CharLiteral                 = cyan
withHighlight StringLiteral               = cyan
withHighlight Constructor                 = bold
withHighlight ReservedOperator            = yellow
withHighlight ConstructorOperator         = yellow
withHighlight ReservedConstructorOperator = yellow
withHighlight _                           = id

-- | A 'HighlightedRope' is a 'Rope' with an associated 'IntervalMap' full of highlighted regions.
data HighlightedRope = HighlightedRope
  { _ropeHighlights :: !(IM.IntervalMap Delta Highlight)
  , _ropeContent    :: {-# UNPACK #-} !Rope
  }

makeClassy ''HighlightedRope

instance HasDelta HighlightedRope where
  delta = delta . _ropeContent

instance HasBytes HighlightedRope where
  bytes = bytes . _ropeContent

instance Semigroup HighlightedRope where
  HighlightedRope h bs <> HighlightedRope h' bs' = HighlightedRope (h `union` IM.offset (delta bs) h') (bs <> bs')

instance Monoid HighlightedRope where
  mappend = (<>)
  mempty = HighlightedRope mempty mempty

data Located a = a :@ {-# UNPACK #-} !Int64
infix 5 :@
instance Eq (Located a) where
  _ :@ m == _ :@ n = m == n
instance Ord (Located a) where
  compare (_ :@ m) (_ :@ n) = compare m n

instance Pretty HighlightedRope where
  pretty (HighlightedRope intervals r) = go mempty lbs boundaries where
    lbs = L.fromChunks [bs | Strand bs _ <- F.toList (strands r)]
    ints = intersections mempty (delta r) intervals
    boundaries = sort [ i | (Interval lo hi, _) <- ints, i <- [ lo, hi ] ]
    dominated l h = Prelude.foldr (fmap . withHighlight . snd) id (dominators l h intervals)
    go l cs [] = dominated l (delta r) $ pretty (LazyUTF8.toString cs)
    go l cs (h:es) = dominated l h (pretty (LazyUTF8.toString om)) <> go h nom es
      where (om,nom) = L.splitAt (fromIntegral (bytes h - bytes l)) cs

-- | Represents a source file like an HsColour rendered document
data HighlightDoc = HighlightDoc
  { _docTitle   :: String
  , _docCss     :: String -- href for the css file
  , _docContent :: HighlightedRope
  }

makeClassy ''HighlightDoc

-- | Generate an HTML document from a title and a 'HighlightedRope'.
doc :: String -> HighlightedRope -> HighlightDoc
doc t r = HighlightDoc t "trifecta.css" r
