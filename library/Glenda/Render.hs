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
  , renderIntLit
  , renderDecimalLit
  , renderNonZeroDecimalDigit
  , renderOctalLit
  , renderHexLit
  , renderX
  , renderFloatLit
  , renderDecimals
  , renderExponent
  , renderE
  , renderSign
  , renderImaginaryLit
  , renderRuneLit
  , renderUnicodeValue
  , renderByteValue
  , renderOctalByteValue
  , renderHexByteValue
  , renderLittleUValue
  , renderBigUValue
  , renderEscapedChar
  , renderStringLit
  , renderRawStringLit
  , renderInterpretedStringLit
  , renderQualifiedIdent
  , renderPackageClause
  , renderPackageName
  , renderEmptyStmt
  , renderFallthroughStmt
  , renderImportDecl
  , renderImportSpec
  , renderImportPath
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

renderIntLit :: Render Go.IntLit
renderIntLit x = case x of
  Go.IntLit_DecimalLit y -> renderDecimalLit y
  Go.IntLit_OctalLit y -> renderOctalLit y
  Go.IntLit_HexLit y -> renderHexLit y

renderDecimalLit :: Render Go.DecimalLit
renderDecimalLit (Go.DecimalLit x y) =
  renderNonZeroDecimalDigit x . renderList renderDecimalDigit y

renderNonZeroDecimalDigit :: Render Go.NonZeroDecimalDigit
renderNonZeroDecimalDigit x = case x of
  Go.NonZeroDecimalDigit_1 -> mappend "1"
  Go.NonZeroDecimalDigit_2 -> mappend "2"
  Go.NonZeroDecimalDigit_3 -> mappend "3"
  Go.NonZeroDecimalDigit_4 -> mappend "4"
  Go.NonZeroDecimalDigit_5 -> mappend "5"
  Go.NonZeroDecimalDigit_6 -> mappend "6"
  Go.NonZeroDecimalDigit_7 -> mappend "7"
  Go.NonZeroDecimalDigit_8 -> mappend "8"
  Go.NonZeroDecimalDigit_9 -> mappend "9"

renderOctalLit :: Render Go.OctalLit
renderOctalLit (Go.OctalLit x) = mappend "0" . renderList renderOctalDigit x

renderHexLit :: Render Go.HexLit
renderHexLit (Go.HexLit x y z) =
  mappend "0" . renderX x . renderHexDigit y . renderList renderHexDigit z

renderX :: Render Go.X
renderX x = case x of
  Go.X_Upper -> mappend "X"
  Go.X_Lower -> mappend "x"

renderFloatLit :: Render Go.FloatLit
renderFloatLit x = case x of
  Go.FloatLit_Trailing y z a ->
    renderDecimals y
    . mappend "."
    . renderMaybe renderDecimals z
    . renderMaybe renderExponent a
  Go.FloatLit_Exponent y z -> renderDecimals y . renderExponent z
  Go.FloatLit_Leading y z ->
    mappend "." . renderDecimals y . renderMaybe renderExponent z

renderMaybe :: Render a -> Render (Maybe a)
renderMaybe f m = maybe id f m

renderDecimals :: Render Go.Decimals
renderDecimals (Go.Decimals x y) =
  renderDecimalDigit x . renderList renderDecimalDigit y

renderExponent :: Render Go.Exponent
renderExponent (Go.Exponent x y z) =
  renderE x . renderMaybe renderSign y . renderDecimals z

renderE :: Render Go.E
renderE x = case x of
  Go.E_Upper -> mappend "E"
  Go.E_Lower -> mappend "e"

renderSign :: Render Go.Sign
renderSign x = case x of
  Go.Sign_Positive -> mappend "+"
  Go.Sign_Negative -> mappend "-"

renderImaginaryLit :: Render Go.ImaginaryLit
renderImaginaryLit x = case x of
  Go.ImaginaryLit_Decimals y -> renderDecimals y . mappend "i"
  Go.ImaginaryLit_FloatLit y -> renderFloatLit y . mappend "i"

renderRuneLit :: Render Go.RuneLit
renderRuneLit x = case x of
  Go.RuneLit_UnicodeValue y -> mappend "'" . renderUnicodeValue y . mappend "'"
  Go.RuneLit_ByteValue y -> mappend "'" . renderByteValue y . mappend "'"

renderUnicodeValue :: Render Go.UnicodeValue
renderUnicodeValue x = case x of
  Go.UnicodeValue_UnicodeChar y -> renderUnicodeChar y
  Go.UnicodeValue_LittleUValue y -> renderLittleUValue y
  Go.UnicodeValue_BigUValue y -> renderBigUValue y
  Go.UnicodeValue_EscapedChar y -> renderEscapedChar y

renderByteValue :: Render Go.ByteValue
renderByteValue x = case x of
  Go.ByteValue_OctalByteValue y -> renderOctalByteValue y
  Go.ByteValue_HexByteValue y -> renderHexByteValue y

renderOctalByteValue :: Render Go.OctalByteValue
renderOctalByteValue (Go.OctalByteValue a b c) = mappend "\\"
  . renderOctalDigit a
  . renderOctalDigit b
  . renderOctalDigit c

renderHexByteValue :: Render Go.HexByteValue
renderHexByteValue (Go.HexByteValue a b) = mappend "\\x"
  . renderHexDigit a
  . renderHexDigit b

renderLittleUValue :: Render Go.LittleUValue
renderLittleUValue (Go.LittleUValue a b c d) = mappend "\\u"
  . renderHexDigit a
  . renderHexDigit b
  . renderHexDigit c
  . renderHexDigit d

renderBigUValue :: Render Go.BigUValue
renderBigUValue (Go.BigUValue a b c d e f g h) = mappend "\\U"
  . renderHexDigit a
  . renderHexDigit b
  . renderHexDigit c
  . renderHexDigit d
  . renderHexDigit e
  . renderHexDigit f
  . renderHexDigit g
  . renderHexDigit h

renderEscapedChar :: Render Go.EscapedChar
renderEscapedChar x = case x of
  Go.EscapedChar_Bell -> mappend "\\a"
  Go.EscapedChar_Backspace -> mappend "\\b"
  Go.EscapedChar_FormFeed -> mappend "\\f"
  Go.EscapedChar_LineFeed -> mappend "\\n"
  Go.EscapedChar_CarriageReturn -> mappend "\\r"
  Go.EscapedChar_HorizontalTab -> mappend "\\t"
  Go.EscapedChar_VerticalTab -> mappend "\\v"
  Go.EscapedChar_Backslash -> mappend "\\\\"
  Go.EscapedChar_SingleQuote -> mappend "\\'"
  Go.EscapedChar_DoubleQuote -> mappend "\\\""

renderStringLit :: Render Go.StringLit
renderStringLit x = case x of
  Go.StringLit_RawStringLit y -> renderRawStringLit y
  Go.StringLit_InterpretedStringLit y -> renderInterpretedStringLit y

renderRawStringLit :: Render Go.RawStringLit
renderRawStringLit (Go.RawStringLit x) =
  mappend "`"
  . renderList (either renderUnicodeChar renderNewline) x
  . mappend "`"

renderInterpretedStringLit :: Render Go.InterpretedStringLit
renderInterpretedStringLit (Go.InterpretedStringLit x) =
  mappend "\""
  . renderList (either renderUnicodeValue renderByteValue) x
  . mappend "\""

renderQualifiedIdent :: Render Go.QualifiedIdent
renderQualifiedIdent (Go.QualifiedIdent x y) =
  renderPackageName x
  . mappend "."
  . renderIdentifier y

renderPackageClause :: Render Go.PackageClause
renderPackageClause (Go.PackageClause x) =
  mappend "package " . renderPackageName x

renderPackageName :: Render Go.PackageName
renderPackageName (Go.PackageName x) = renderIdentifier x

renderEmptyStmt :: Render Go.EmptyStmt
renderEmptyStmt Go.EmptyStmt = id

renderFallthroughStmt :: Render Go.FallthroughStmt
renderFallthroughStmt Go.FallthroughStmt = mappend "fallthrough"

renderImportDecl :: Render Go.ImportDecl
renderImportDecl x = case x of
  Go.ImportDecl_One y -> mappend "import " . renderImportSpec y
  Go.ImportDecl_Many y ->
    mappend "import ("
    . renderList (\ z -> renderImportSpec z . mappend ";") y
    . mappend ")"

renderImportSpec :: Render Go.ImportSpec
renderImportSpec x = case x of
  Go.ImportSpec_Unqualified y -> mappend ". " . renderImportPath y
  Go.ImportSpec_Explicit y z ->
    renderPackageName y . mappend " " . renderImportPath z
  Go.ImportSpec_Implicit y -> renderImportPath y

renderImportPath :: Render Go.ImportPath
renderImportPath (Go.ImportPath x) = renderStringLit x
