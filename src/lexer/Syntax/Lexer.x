{

{-# language FlexibleInstances #-}
{-# language MultiParamTypeClasses #-}
{-# language LambdaCase #-}
{-# language OverloadedStrings #-}
{-# language TypeFamilies #-}
{-# language TemplateHaskell #-}

---------------------------------------------------------------------------------
-- |
-- Copyright :  (c) Edward Kmett 2017-2018
-- License   :  BSD-2-Clause OR Apache-2.0
-- Maintainer:  Edward Kmett <ekmett@gmail.com>
-- Stability :  experimental
-- Portability: non-portable
--
---------------------------------------------------------------------------------

module Syntax.Lexer
  ( lex
  ) where

import Data.Char
import Data.Default
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.Read as Read
import Data.Text.Unsafe as Text
import Prelude hiding (lex)
import Text.Read (readEither)

import Relative.Delta
import Relative.Located
import Syntax.Alex
import Syntax.FromText
import Syntax.Name
import Syntax.Token
import Syntax.Dyck

}

$whitechar = [ \t\n\r\f\v]
$special   = [\,\;\`]
$closing   = [\)\]\}]
$opening   = [\(\[\{]
$digit     = 0-9
$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special $closing $opening \_\:\"\']
$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]
$graphic   = [$small $large $symbol $digit $special $closing $opening \:\"\']
$octit     = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]
$charesc   = [abfnrtv\\\"\'\&]

@keyid
  = as | case | class | data | default | deriving | else | hiding | if
  | import | in | infix | infixl | infixr | instance | module | newtype
  | qualified | then | type

@keyop
  = ".." | ":" | "::" | "=" | \\ | "|" | "<-" | "->" | "@" | "~" | "=>"

@varid  = $small $idchar*
@conid  = $large $idchar*
@varop = $symbol $symchar*
@conop = \: $symchar*
@decimal = $digit+
@octal = $octit+
@hexadecimal = $hexit+
@exponent = [eE] [\-\+] @decimal
@sign = [\-\+]?

$cntrl = [$large \@\[\\\]\^\_]

@ascii
  = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
  | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
  | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
  | SUB | ESC | FS | GS | RS | US | SP | DEL

@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@string  = $graphic # [\"\\] | " " | @escape

haskell :-
<0> $white+ { skip }
<0> "--"\-*[^$symbol].* { skip }
<0> $special { tok }
<0> $closing { closing }
<0> $opening { opening }
<0> do { layoutKeyword LDo }
<0> let { layoutKeyword LLet }
<0> of { layoutKeyword LOf }
<0> where { layoutKeyword LWhere }
<0> @keyid { keyword }
<0> (@conid \.)+ @varid { qualified False False }
<0> (@conid \.)+ @conid { qualified False True }
<0> @varid { unqualified False False }
<0> @conid { unqualified False True }
<0> @keyop { tok }
<0> (@conid \.)+ @varop { qualified True False }
<0> (@conid \.)+ @conop { qualified True True }
<0> @varop { unqualified True False }
<0> @conop { unqualified True True }
<0> @decimal { reader 0 TokenInteger (Read.signed Read.decimal) }
<0> 0[oO] @octal { octal }
<0> 0[xX] @hexadecimal { reader 2 TokenInteger Read.hexadecimal }
<0> @sign @decimal \. @decimal @exponent?
  | @sign @decimal @exponent { reader 0 TokenDouble (Read.signed Read.double) }
<0> \' ($graphic # [\'\\] | " " | @escape) \' { literal TokenChar }
<0> \" @string* \" { literal TokenString }

{

pair :: Char -> Pair
pair '(' = Paren
pair ')' = Paren
pair '[' = Bracket
pair ']' = Bracket
pair '{' = Brace
pair '}' = Brace
pair _ = error "bad pair"

trim :: Int -> Text -> Int -> Text
trim d t l = Text.takeWord16 l $ Text.dropWord16 d t

cap :: String -> String
cap (x:xs) = toUpper x : xs
cap [] = []

type Action = Dyck -> Int -> Text -> Int -> Dyck

skip :: Action
skip xs _ _ _ = xs

qualified :: Bool -> Bool -> Action
qualified o c xs d t ln = case Text.breakOnEnd "." (trim d t ln) of
  (l,r) -> tokenFlat xs $ TokenName (Delta d) $ Qualified o c (Text.init l) r

unqualified :: Bool -> Bool -> Action
unqualified o c xs d t l = tokenFlat xs $ TokenName (Delta d) $ Unqualified o c (trim d t l)

tok :: Action
tok xs d t len = tokenFlat xs $ Token (Delta d) $ Text.takeWord16 len $ Text.dropWord16 d t

keyword :: Action
keyword xs d t l = tokenFlat xs $ case readEither $ 'K' : cap (Text.unpack $ trim d t l) of
  Right kw -> TokenKeyword (Delta d) kw
  Left e -> lexicalError (Delta d) e

layoutKeyword :: LayoutMode -> Action
layoutKeyword i xs d t l = layoutToken xs i $ flat $ case readEither $ 'K' : cap (Text.unpack $ trim d t l) of
  Right kw -> TokenKeyword (Delta d) kw
  Left e -> lexicalError (Delta d) e

opening :: Action
opening xs d t _ = open xs $ Located (Delta d) $ case Text.iter t d of Text.Iter c _ -> pair c

closing :: Action
closing xs d t _ = close xs $ Located (Delta d) $ case Text.iter t d of Text.Iter c _ -> pair c

literal :: Read a => (Delta -> a -> TokenFlat) -> Action
literal f xs d t l = tokenFlat xs $ case readEither $ Text.unpack $ trim d t l of
  Left e  -> lexicalError (Delta d) e
  Right a -> f (Delta d) a

reader :: Int -> (Delta -> a -> TokenFlat) -> Read.Reader a -> Action
reader o f p xs d t l = tokenFlat xs $ case p $ trim (d+o) t (l-o) of
  Left e       -> lexicalError (Delta d) e
  Right (a, _) -> f (Delta d) a

octal :: Action
octal = reader 2 TokenInteger $ \ txt -> Right (Text.foldl' (\n d -> n*8 + fromIntegral (digitToInt d)) 0 txt, "")

lex :: Text -> Dyck
lex t0 = go t0 (fromText t0) def where
  go t inp xs = case alexScan inp 0 of
    AlexEOF              -> xs
    AlexError inp'       -> go t inp' $ tokenFlat xs $ lexicalError (Delta $ alexInputDelta inp) "lexical error"
    AlexSkip inp' _      -> go t inp' xs
    AlexToken inp' l act -> go t inp' $ act xs (alexInputDelta inp) t l

}
