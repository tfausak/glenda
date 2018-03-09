-- | This module defines the types used to represent the Go programming
-- language.
module Glenda.Language
  ( Newline(..)
  , UnicodeChar(..)
  , UnicodeLetter(..)
  , UnicodeDigit(..)
  , Letter(..)
  , DecimalDigit(..)
  , OctalDigit(..)
  , HexDigit(..)
  ) where

data Newline
  = Newline
  deriving (Eq, Show)

newtype UnicodeChar
  = UnicodeChar Char
  deriving (Eq, Show)

newtype UnicodeLetter
  = UnicodeLetter Char
  deriving (Eq, Show)

newtype UnicodeDigit
  = UnicodeDigit Char
  deriving (Eq, Show)

data Letter
  = Letter_UnicodeLetter UnicodeLetter
  | Letter_Underscore
  deriving (Eq, Show)

data DecimalDigit
  = DecimalDigit_0
  | DecimalDigit_1
  | DecimalDigit_2
  | DecimalDigit_3
  | DecimalDigit_4
  | DecimalDigit_5
  | DecimalDigit_6
  | DecimalDigit_7
  | DecimalDigit_8
  | DecimalDigit_9
  deriving (Eq, Show)

data OctalDigit
  = OctalDigit_0
  | OctalDigit_1
  | OctalDigit_2
  | OctalDigit_3
  | OctalDigit_4
  | OctalDigit_5
  | OctalDigit_6
  | OctalDigit_7
  deriving (Eq, Show)

data HexDigit
  = HexDigit_0
  | HexDigit_1
  | HexDigit_2
  | HexDigit_3
  | HexDigit_4
  | HexDigit_5
  | HexDigit_6
  | HexDigit_7
  | HexDigit_8
  | HexDigit_9
  | HexDigit_A
  | HexDigit_a
  | HexDigit_B
  | HexDigit_b
  | HexDigit_C
  | HexDigit_c
  | HexDigit_D
  | HexDigit_d
  | HexDigit_E
  | HexDigit_e
  | HexDigit_F
  | HexDigit_f
  deriving (Eq, Show)
