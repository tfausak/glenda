-- | This module defines the functions used to render (pretty print) the Go
-- programming language.
module Glenda.Render
  ( Render
  , runRender
  , renderNewline
  , renderUnicodeChar
  , renderUnicodeLetter
  , renderUnicodeDigit
  ) where

import qualified Glenda.Language as Go

-- | See 'ShowS' for details. Use 'runRender' to convert directly into a
-- string.
type Render input = input -> ShowS

-- | Converts a value to a string..
runRender :: Render input -> input -> String
runRender render input = render input ""

renderNewline :: Render Go.Newline
renderNewline Go.Newline = mappend "\n"

renderUnicodeChar :: Render Go.UnicodeChar
renderUnicodeChar (Go.UnicodeChar x) = mappend [x]

renderUnicodeLetter :: Render Go.UnicodeLetter
renderUnicodeLetter (Go.UnicodeLetter x) = mappend [x]

renderUnicodeDigit :: Render Go.UnicodeDigit
renderUnicodeDigit (Go.UnicodeDigit x) = mappend [x]
