-- | This module defines the types used to represent the Go programming
-- language.
module Glenda.Language
  ( Newline(..)
  , UnicodeChar(..)
  , UnicodeLetter(..)
  , UnicodeDigit(..)
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
