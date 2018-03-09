-- | This module defines the functions used to render (pretty print) the Go
-- programming language.
module Glenda.Render
  ( Render
  , runRender
  , renderNewline
  , renderUnicodeChar
  , renderUnicodeLetter
  , renderUnicodeDigit
  , renderLetter
  , renderDecimalDigit
  , renderOctalDigit
  , renderHexDigit
  , renderIdentifier
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

renderLetter :: Render Go.Letter
renderLetter x = case x of
  Go.Letter_UnicodeLetter y -> renderUnicodeLetter y
  Go.Letter_Underscore -> mappend "_"

renderDecimalDigit :: Render Go.DecimalDigit
renderDecimalDigit x = case x of
  Go.DecimalDigit_0 -> mappend "0"
  Go.DecimalDigit_1 -> mappend "1"
  Go.DecimalDigit_2 -> mappend "2"
  Go.DecimalDigit_3 -> mappend "3"
  Go.DecimalDigit_4 -> mappend "4"
  Go.DecimalDigit_5 -> mappend "5"
  Go.DecimalDigit_6 -> mappend "6"
  Go.DecimalDigit_7 -> mappend "7"
  Go.DecimalDigit_8 -> mappend "8"
  Go.DecimalDigit_9 -> mappend "9"

renderOctalDigit :: Render Go.OctalDigit
renderOctalDigit x = case x of
  Go.OctalDigit_0 -> mappend "0"
  Go.OctalDigit_1 -> mappend "1"
  Go.OctalDigit_2 -> mappend "2"
  Go.OctalDigit_3 -> mappend "3"
  Go.OctalDigit_4 -> mappend "4"
  Go.OctalDigit_5 -> mappend "5"
  Go.OctalDigit_6 -> mappend "6"
  Go.OctalDigit_7 -> mappend "7"

renderHexDigit :: Render Go.HexDigit
renderHexDigit x = case x of
  Go.HexDigit_0 -> mappend "0"
  Go.HexDigit_1 -> mappend "1"
  Go.HexDigit_2 -> mappend "2"
  Go.HexDigit_3 -> mappend "3"
  Go.HexDigit_4 -> mappend "4"
  Go.HexDigit_5 -> mappend "5"
  Go.HexDigit_6 -> mappend "6"
  Go.HexDigit_7 -> mappend "7"
  Go.HexDigit_8 -> mappend "8"
  Go.HexDigit_9 -> mappend "9"
  Go.HexDigit_A -> mappend "A"
  Go.HexDigit_a -> mappend "a"
  Go.HexDigit_B -> mappend "B"
  Go.HexDigit_b -> mappend "b"
  Go.HexDigit_C -> mappend "C"
  Go.HexDigit_c -> mappend "c"
  Go.HexDigit_D -> mappend "D"
  Go.HexDigit_d -> mappend "d"
  Go.HexDigit_E -> mappend "E"
  Go.HexDigit_e -> mappend "e"
  Go.HexDigit_F -> mappend "F"
  Go.HexDigit_f -> mappend "f"

renderIdentifier :: Render Go.Identifier
renderIdentifier (Go.Identifier x y) =
  renderLetter x . renderList (either renderLetter renderUnicodeDigit) y

renderList :: Render element -> Render [element]
renderList f xs = foldr (\ x g -> f x . g) id xs
