-- | This module defines the functions used to parse the Go programming
-- language.
module Glenda.Parse
  ( Parse
  , runParse
  , parseNewline
  , parseUnicodeChar
  , parseUnicodeLetter
  , parseUnicodeDigit
  ) where

import Data.Functor (($>))

import qualified Data.Char as Char
import qualified Data.Maybe as Maybe
import qualified Glenda.Language as Go
import qualified Text.ParserCombinators.ReadP as Parse

-- | See 'Parse.ReadP' for details. Use 'runParse' to convert directly from a
-- string.
type Parse output = Parse.ReadP output

-- | Parses a string to a value.
runParse :: Parse output -> String -> Maybe output
runParse parse
  = Maybe.listToMaybe
  . map fst
  . filter (null . snd)
  . Parse.readP_to_S parse

parseNewline :: Parse Go.Newline
parseNewline = Parse.char '\n' $> Go.Newline

parseUnicodeChar :: Parse Go.UnicodeChar
parseUnicodeChar = Go.UnicodeChar <$> Parse.satisfy (/= '\n')

parseUnicodeLetter :: Parse Go.UnicodeLetter
parseUnicodeLetter = Go.UnicodeLetter <$> Parse.satisfy Char.isLetter

parseUnicodeDigit :: Parse Go.UnicodeDigit
parseUnicodeDigit = Go.UnicodeDigit <$> Parse.satisfy Char.isNumber
