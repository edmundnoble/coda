-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2011-2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
--
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-- For a short introduction, see the "Syntax.Trifecta.Tutorial" module.
----------------------------------------------------------------------------
module Syntax.Trifecta
  ( module Syntax.Trifecta.Rendering
  , module Syntax.Trifecta.Highlight
  , module Syntax.Trifecta.Parser
  , module Syntax.Trifecta.Combinators
  , module Syntax.Trifecta.Result
  , module Syntax.Trifecta.Rope
  , module Text.Parser.Combinators
  , module Text.Parser.Char
  , module Text.Parser.Token
  ) where

import Text.Parser.Char
import Text.Parser.Combinators
import Text.Parser.Token
import Syntax.Trifecta.Combinators
import Syntax.Trifecta.Highlight
import Syntax.Trifecta.Parser
import Syntax.Trifecta.Rendering
import Syntax.Trifecta.Result
import Syntax.Trifecta.Rope
