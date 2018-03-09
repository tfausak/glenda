-- | This module defines the functions used to parse the Go programming
-- language.
module Glenda.Parse
  ( Parse
  , runParse
  , parseNewline
  , parseUnicodeChar
  , parseUnicodeLetter
  , parseUnicodeDigit
  , parseLetter
  , parseDecimalDigit
  , parseOctalDigit
  , parseHexDigit
  , parseComment
  , parseLineComment
  , parseGeneralComment
  , parseWhiteSpace
  , parseIdentifier
  ) where

import Data.Functor ((<$))

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
parseNewline = Go.Newline <$ Parse.char '\n'

parseUnicodeChar :: Parse Go.UnicodeChar
parseUnicodeChar = Go.UnicodeChar <$> Parse.satisfy (/= '\n')

parseUnicodeLetter :: Parse Go.UnicodeLetter
parseUnicodeLetter = Go.UnicodeLetter <$> Parse.satisfy Char.isLetter

parseUnicodeDigit :: Parse Go.UnicodeDigit
parseUnicodeDigit = Go.UnicodeDigit <$> Parse.satisfy Char.isNumber

parseLetter :: Parse Go.Letter
parseLetter = Parse.choice
  [ Go.Letter_UnicodeLetter <$> parseUnicodeLetter
  , Go.Letter_Underscore <$ Parse.char '_'
  ]

parseDecimalDigit :: Parse Go.DecimalDigit
parseDecimalDigit = Parse.choice
  [ Go.DecimalDigit_0 <$ Parse.char '0'
  , Go.DecimalDigit_1 <$ Parse.char '1'
  , Go.DecimalDigit_2 <$ Parse.char '2'
  , Go.DecimalDigit_3 <$ Parse.char '3'
  , Go.DecimalDigit_4 <$ Parse.char '4'
  , Go.DecimalDigit_5 <$ Parse.char '5'
  , Go.DecimalDigit_6 <$ Parse.char '6'
  , Go.DecimalDigit_7 <$ Parse.char '7'
  , Go.DecimalDigit_8 <$ Parse.char '8'
  , Go.DecimalDigit_9 <$ Parse.char '9'
  ]

parseOctalDigit :: Parse Go.OctalDigit
parseOctalDigit = Parse.choice
  [ Go.OctalDigit_0 <$ Parse.char '0'
  , Go.OctalDigit_1 <$ Parse.char '1'
  , Go.OctalDigit_2 <$ Parse.char '2'
  , Go.OctalDigit_3 <$ Parse.char '3'
  , Go.OctalDigit_4 <$ Parse.char '4'
  , Go.OctalDigit_5 <$ Parse.char '5'
  , Go.OctalDigit_6 <$ Parse.char '6'
  , Go.OctalDigit_7 <$ Parse.char '7'
  ]

parseHexDigit :: Parse Go.HexDigit
parseHexDigit = Parse.choice
  [ Go.HexDigit_0 <$ Parse.char '0'
  , Go.HexDigit_1 <$ Parse.char '1'
  , Go.HexDigit_2 <$ Parse.char '2'
  , Go.HexDigit_3 <$ Parse.char '3'
  , Go.HexDigit_4 <$ Parse.char '4'
  , Go.HexDigit_5 <$ Parse.char '5'
  , Go.HexDigit_6 <$ Parse.char '6'
  , Go.HexDigit_7 <$ Parse.char '7'
  , Go.HexDigit_8 <$ Parse.char '8'
  , Go.HexDigit_A <$ Parse.char 'A'
  , Go.HexDigit_a <$ Parse.char 'a'
  , Go.HexDigit_B <$ Parse.char 'B'
  , Go.HexDigit_b <$ Parse.char 'b'
  , Go.HexDigit_C <$ Parse.char 'C'
  , Go.HexDigit_c <$ Parse.char 'c'
  , Go.HexDigit_D <$ Parse.char 'D'
  , Go.HexDigit_d <$ Parse.char 'd'
  , Go.HexDigit_E <$ Parse.char 'E'
  , Go.HexDigit_e <$ Parse.char 'e'
  , Go.HexDigit_F <$ Parse.char 'F'
  , Go.HexDigit_f <$ Parse.char 'f'
  ]

parseComment :: Parse Char
parseComment = Parse.choice
  [ parseLineComment
  , parseGeneralComment
  ]

parseLineComment :: Parse Char
parseLineComment = do
  _ <- Parse.string "//"
  _ <- Parse.manyTill Parse.get (Parse.char '\n')
  pure '\n'

parseGeneralComment :: Parse Char
parseGeneralComment = do
  _ <- Parse.string "/*"
  x <- Parse.manyTill Parse.get (Parse.string "*/")
  pure (if elem '\n' x then '\n' else ' ')

parseWhiteSpace :: Parse String
parseWhiteSpace = Parse.many (Parse.choice
  [ Parse.char ' '
  , Parse.char '\t'
  , Parse.char '\r'
  , Parse.char '\n'
  , parseComment
  ])

parseIdentifier :: Parse Go.Identifier
parseIdentifier = Go.Identifier
  <$> parseLetter
  <*> Parse.many (parseEither parseLetter parseUnicodeDigit)

parseEither :: Parse left -> Parse right -> Parse (Either left right)
parseEither parseLeft parseRight = Parse.choice
  [ Left <$> parseLeft
  , Right <$> parseRight
  ]
