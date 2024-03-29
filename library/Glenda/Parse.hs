-- | This module defines the functions used to parse the Go programming
-- language. These aren't particularly fast, they don't provide good errors,
-- and they require explicit semicolons. In other words, they're not exactly
-- ready for production.
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
  , parseIntLit
  , parseDecimalLit
  , parseNonZeroDecimalDigit
  , parseOctalLit
  , parseHexLit
  , parseX
  , parseFloatLit
  , parseDecimals
  , parseExponent
  , parseE
  , parseSign
  , parseImaginaryLit
  , parseRuneLit
  , parseUnicodeValue
  , parseByteValue
  , parseOctalByteValue
  , parseHexByteValue
  , parseLittleUValue
  , parseBigUValue
  , parseEscapedChar
  , parseStringLit
  , parseRawStringLit
  , parseInterpretedStringLit
  , parseTypeName
  , parseIdentifierList
  , parseFunctionName
  , parseBasicLit
  , parseOperandName
  , parseQualifiedIdent
  , parseFieldName
  , parseSelector
  , parseBinaryOp
  , parseRelOp
  , parseAddOp
  , parseMulOp
  , parseUnaryOp
  , parsePackageClause
  , parsePackageName
  , parseEmptyStmt
  , parseLabel
  , parseAssignOp
  , parseBreakStmt
  , parseContinueStmt
  , parseGotoStmt
  , parseFallthroughStmt
  , parseImportDecl
  , parseImportSpec
  , parseImportPath
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

parseToken :: Parse a -> Parse a
parseToken parse = parse <* parseWhiteSpace

parseIdentifier :: Parse Go.Identifier
parseIdentifier = parseToken (Go.Identifier
  <$> parseLetter
  <*> Parse.many (parseEither parseLetter parseUnicodeDigit))

parseEither :: Parse left -> Parse right -> Parse (Either left right)
parseEither parseLeft parseRight = Parse.choice
  [ Left <$> parseLeft
  , Right <$> parseRight
  ]

parseIntLit :: Parse Go.IntLit
parseIntLit = parseToken (Parse.choice
  [ Go.IntLit_DecimalLit <$> parseDecimalLit
  , Go.IntLit_OctalLit <$> parseOctalLit
  , Go.IntLit_HexLit <$> parseHexLit
  ])

parseDecimalLit :: Parse Go.DecimalLit
parseDecimalLit = Go.DecimalLit
  <$> parseNonZeroDecimalDigit
  <*> Parse.many parseDecimalDigit

parseNonZeroDecimalDigit :: Parse Go.NonZeroDecimalDigit
parseNonZeroDecimalDigit = Parse.choice
  [ Go.NonZeroDecimalDigit_1 <$ Parse.char '1'
  , Go.NonZeroDecimalDigit_2 <$ Parse.char '2'
  , Go.NonZeroDecimalDigit_3 <$ Parse.char '3'
  , Go.NonZeroDecimalDigit_4 <$ Parse.char '4'
  , Go.NonZeroDecimalDigit_5 <$ Parse.char '5'
  , Go.NonZeroDecimalDigit_6 <$ Parse.char '6'
  , Go.NonZeroDecimalDigit_7 <$ Parse.char '7'
  , Go.NonZeroDecimalDigit_8 <$ Parse.char '8'
  , Go.NonZeroDecimalDigit_9 <$ Parse.char '9'
  ]

parseOctalLit :: Parse Go.OctalLit
parseOctalLit = Parse.char '0' *> (Go.OctalLit <$> Parse.many parseOctalDigit)

parseHexLit :: Parse Go.HexLit
parseHexLit = Parse.char '0' *> (Go.HexLit
  <$> parseX
  <*> parseHexDigit
  <*> Parse.many parseHexDigit)

parseX :: Parse Go.X
parseX = Parse.choice
  [ Go.X_Upper <$ Parse.char 'X'
  , Go.X_Lower <$ Parse.char 'x'
  ]

parseFloatLit :: Parse Go.FloatLit
parseFloatLit = parseToken (Parse.choice
  [ Go.FloatLit_Trailing
    <$> (parseDecimals <* Parse.char '.')
    <*> parseMaybe parseDecimals
    <*> parseMaybe parseExponent
  , Go.FloatLit_Exponent <$> parseDecimals <*> parseExponent
  , Go.FloatLit_Leading <$> parseDecimals <*> parseMaybe parseExponent
  ])

parseMaybe :: Parse a -> Parse (Maybe a)
parseMaybe parse = Parse.option Nothing (Just <$> parse)

parseDecimals :: Parse Go.Decimals
parseDecimals = Go.Decimals
  <$> parseDecimalDigit
  <*> Parse.many parseDecimalDigit

parseExponent :: Parse Go.Exponent
parseExponent = Go.Exponent
  <$> parseE
  <*> parseMaybe parseSign
  <*> parseDecimals

parseE :: Parse Go.E
parseE = Parse.choice
  [ Go.E_Upper <$ Parse.char 'E'
  , Go.E_Lower <$ Parse.char 'e'
  ]

parseSign :: Parse Go.Sign
parseSign = Parse.choice
  [ Go.Sign_Positive <$ Parse.char '+'
  , Go.Sign_Negative <$ Parse.char '-'
  ]

parseImaginaryLit :: Parse Go.ImaginaryLit
parseImaginaryLit = parseToken (Parse.choice
  [ Go.ImaginaryLit_Decimals <$> (parseDecimals <* Parse.char 'i')
  , Go.ImaginaryLit_FloatLit <$> (parseFloatLit <* Parse.char 'i')
  ])

parseRuneLit :: Parse Go.RuneLit
parseRuneLit = parseToken
  (Parse.between (Parse.char '\'') (Parse.char '\'') (Parse.choice
    [ Go.RuneLit_UnicodeValue <$> parseUnicodeValue
    , Go.RuneLit_ByteValue <$> parseByteValue
    ]))

parseUnicodeValue :: Parse Go.UnicodeValue
parseUnicodeValue = Parse.choice
  [ Go.UnicodeValue_UnicodeChar <$> parseUnicodeChar
  , Go.UnicodeValue_LittleUValue <$> parseLittleUValue
  , Go.UnicodeValue_BigUValue <$> parseBigUValue
  , Go.UnicodeValue_EscapedChar <$> parseEscapedChar
  ]

parseByteValue :: Parse Go.ByteValue
parseByteValue = Parse.choice
  [ Go.ByteValue_OctalByteValue <$> parseOctalByteValue
  , Go.ByteValue_HexByteValue <$> parseHexByteValue
  ]

parseOctalByteValue :: Parse Go.OctalByteValue
parseOctalByteValue = Parse.char '\\' *> (Go.OctalByteValue
  <$> parseOctalDigit
  <*> parseOctalDigit
  <*> parseOctalDigit)

parseHexByteValue :: Parse Go.HexByteValue
parseHexByteValue = Parse.string "\\x" *> (Go.HexByteValue
  <$> parseHexDigit
  <*> parseHexDigit)

parseLittleUValue :: Parse Go.LittleUValue
parseLittleUValue = Parse.string "\\u" *> (Go.LittleUValue
  <$> parseHexDigit
  <*> parseHexDigit
  <*> parseHexDigit
  <*> parseHexDigit)

parseBigUValue :: Parse Go.BigUValue
parseBigUValue = Parse.string "\\U" *> (Go.BigUValue
  <$> parseHexDigit
  <*> parseHexDigit
  <*> parseHexDigit
  <*> parseHexDigit
  <*> parseHexDigit
  <*> parseHexDigit
  <*> parseHexDigit
  <*> parseHexDigit)

parseEscapedChar :: Parse Go.EscapedChar
parseEscapedChar = Parse.choice
  [ Go.EscapedChar_Bell <$ Parse.string "\\a"
  , Go.EscapedChar_Backspace <$ Parse.string "\\b"
  , Go.EscapedChar_FormFeed <$ Parse.string "\\f"
  , Go.EscapedChar_LineFeed <$ Parse.string "\\n"
  , Go.EscapedChar_CarriageReturn <$ Parse.string "\\r"
  , Go.EscapedChar_HorizontalTab <$ Parse.string "\\t"
  , Go.EscapedChar_VerticalTab <$ Parse.string "\\v"
  , Go.EscapedChar_Backslash <$ Parse.string "\\\\"
  , Go.EscapedChar_SingleQuote <$ Parse.string "\\'"
  , Go.EscapedChar_DoubleQuote <$ Parse.string "\\\""
  ]

parseStringLit :: Parse Go.StringLit
parseStringLit = parseToken (Parse.choice
  [ Go.StringLit_RawStringLit <$> parseRawStringLit
  , Go.StringLit_InterpretedStringLit <$> parseInterpretedStringLit
  ])

parseRawStringLit :: Parse Go.RawStringLit
parseRawStringLit = Parse.char '`' *> (Go.RawStringLit
  <$> Parse.manyTill
    (parseEither parseUnicodeChar parseNewline)
    (Parse.char '`'))

parseInterpretedStringLit :: Parse Go.InterpretedStringLit
parseInterpretedStringLit = Parse.char '"' *> (Go.InterpretedStringLit
  <$> Parse.manyTill
    (parseEither parseUnicodeValue parseByteValue)
    (Parse.char '"'))

parseTypeName :: Parse Go.TypeName
parseTypeName = Parse.choice
  [ Go.TypeName_Unqualified <$> parseIdentifier
  , Go.TypeName_Qualified <$> parseQualifiedIdent
  ]

parseIdentifierList :: Parse Go.IdentifierList
parseIdentifierList = Go.IdentifierList
  <$> parseIdentifier
  <*> Parse.many (parseToken (Parse.char ',') *> parseIdentifier)

parseFunctionName :: Parse Go.FunctionName
parseFunctionName = Go.FunctionName <$> parseIdentifier

parseBasicLit :: Parse Go.BasicLit
parseBasicLit = Parse.choice
  [ Go.BasicLit_IntLit <$> parseIntLit
  , Go.BasicLit_FloatLit <$> parseFloatLit
  , Go.BasicLit_ImaginaryLit <$> parseImaginaryLit
  , Go.BasicLit_RuneLit <$> parseRuneLit
  , Go.BasicLit_StringLit <$> parseStringLit
  ]

parseOperandName :: Parse Go.OperandName
parseOperandName = Parse.choice
  [ Go.OperandName_Unqualified <$> parseIdentifier
  , Go.OperandName_Qualified <$> parseQualifiedIdent
  ]

parseQualifiedIdent :: Parse Go.QualifiedIdent
parseQualifiedIdent = Go.QualifiedIdent
  <$> (parsePackageName <* Parse.char '.')
  <*> parseIdentifier

parseFieldName :: Parse Go.FieldName
parseFieldName = Go.FieldName <$> parseIdentifier

parseSelector :: Parse Go.Selector
parseSelector = parseToken (Parse.char '.') *> (Go.Selector <$> parseIdentifier)

parseBinaryOp :: Parse Go.BinaryOp
parseBinaryOp = Parse.choice
  [ Go.BinaryOp_ConditionalOr <$ parseToken (Parse.string "||")
  , Go.BinaryOp_ConditionalAnd <$ parseToken (Parse.string "&&")
  , Go.BinaryOp_RelOp <$> parseToken parseRelOp
  , Go.BinaryOp_AddOp <$> parseToken parseAddOp
  , Go.BinaryOp_MulOp <$> parseToken parseMulOp
  ]

parseRelOp :: Parse Go.RelOp
parseRelOp = Parse.choice
  [ Go.RelOp_Equal <$ Parse.string "=="
  , Go.RelOp_NotEqual <$ Parse.string "!="
  , Go.RelOp_Less <$ Parse.string "<"
  , Go.RelOp_LessOrEqual <$ Parse.string "<="
  , Go.RelOp_Greater <$ Parse.string ">"
  , Go.RelOp_GreaterOrEqual <$ Parse.string ">="
  ]

parseAddOp :: Parse Go.AddOp
parseAddOp = Parse.choice
  [ Go.AddOp_Sum <$ Parse.string "+"
  , Go.AddOp_Difference <$ Parse.string "-"
  , Go.AddOp_BitwiseOr <$ Parse.string "|"
  , Go.AddOp_BitwiseXor <$ Parse.string "^"
  ]

parseMulOp :: Parse Go.MulOp
parseMulOp = Parse.choice
  [ Go.MulOp_Product <$ Parse.string "*"
  , Go.MulOp_Quotient <$ Parse.string "/"
  , Go.MulOp_Remainder <$ Parse.string "%"
  , Go.MulOp_LeftShift <$ Parse.string "<<"
  , Go.MulOp_RightShift <$ Parse.string ">>"
  , Go.MulOp_BitwiseAnd <$ Parse.string "&"
  , Go.MulOp_BitClear <$ Parse.string "&^"
  ]

parseUnaryOp :: Parse Go.UnaryOp
parseUnaryOp = Parse.choice
  [ Go.UnaryOp_Positive <$ parseToken (Parse.string "+")
  , Go.UnaryOp_Negation <$ parseToken (Parse.string "-")
  , Go.UnaryOp_Not <$ parseToken (Parse.string "!")
  , Go.UnaryOp_BitwiseComplement <$ parseToken (Parse.string "^")
  , Go.UnaryOp_Indirection <$ parseToken (Parse.string "*")
  , Go.UnaryOp_Address <$ parseToken (Parse.string "&")
  , Go.UnaryOp_Receive <$ parseToken (Parse.string "<-")
  ]

parsePackageClause :: Parse Go.PackageClause
parsePackageClause = parseToken (Parse.string "package")
  *> (Go.PackageClause <$> parsePackageName)

parsePackageName :: Parse Go.PackageName
parsePackageName = Go.PackageName <$> parseIdentifier

parseEmptyStmt :: Parse Go.EmptyStmt
parseEmptyStmt = Go.EmptyStmt <$ parseWhiteSpace

parseLabel :: Parse Go.Label
parseLabel = Go.Label <$> parseIdentifier

parseAssignOp :: Parse Go.AssignOp
parseAssignOp = Parse.choice
  [ Go.AssignOp_Normal <$ parseToken (Parse.char '=')
  , Go.AssignOp_AddOp <$> (parseAddOp <* parseToken (Parse.char '='))
  , Go.AssignOp_MulOp <$> (parseMulOp <* parseToken (Parse.char '='))
  ]

parseBreakStmt :: Parse Go.BreakStmt
parseBreakStmt = parseToken (Parse.string "break")
  *> (Go.BreakStmt <$> parseMaybe parseLabel)

parseContinueStmt :: Parse Go.ContinueStmt
parseContinueStmt = parseToken (Parse.string "continue")
  *> (Go.ContinueStmt <$> parseMaybe parseLabel)

parseGotoStmt :: Parse Go.GotoStmt
parseGotoStmt = parseToken (Parse.string "goto")
  *> (Go.GotoStmt <$> parseLabel)

parseFallthroughStmt :: Parse Go.FallthroughStmt
parseFallthroughStmt =
  Go.FallthroughStmt <$ parseToken (Parse.string "fallthrough")

parseImportDecl :: Parse Go.ImportDecl
parseImportDecl = Parse.choice
  [ Go.ImportDecl_One
    <$> (parseToken (Parse.string "import") *> parseImportSpec)
  , Go.ImportDecl_Many <$>
    ( parseToken (Parse.string "import")
    *> parseToken (Parse.char '(')
    *> Parse.endBy parseImportSpec (parseToken (Parse.char ';'))
    <* parseToken (Parse.char ')')
    )
  ]

parseImportSpec :: Parse Go.ImportSpec
parseImportSpec = Parse.choice
  [ Go.ImportSpec_Unqualified
    <$> (parseToken (Parse.char '.') *> parseImportPath)
  , Go.ImportSpec_Explicit <$> parsePackageName <*> parseImportPath
  , Go.ImportSpec_Implicit <$> parseImportPath
  ]

parseImportPath :: Parse Go.ImportPath
parseImportPath = Go.ImportPath <$> parseStringLit
