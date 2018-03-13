-- | This module defines the types used to represent the Go programming
-- language. These are a overly specific in order to make parsing and rendering
-- easier.
module Glenda.Language
  ( Newline(..)
  , UnicodeChar(..)
  , UnicodeLetter(..)
  , UnicodeDigit(..)
  , Letter(..)
  , DecimalDigit(..)
  , OctalDigit(..)
  , HexDigit(..)
  , Identifier(..)
  , IntLit(..)
  , DecimalLit(..)
  , NonZeroDecimalDigit(..)
  , OctalLit(..)
  , HexLit(..)
  , X(..)
  , FloatLit(..)
  , Decimals(..)
  , Exponent(..)
  , E(..)
  , Sign(..)
  , ImaginaryLit(..)
  , RuneLit(..)
  , UnicodeValue(..)
  , ByteValue(..)
  , OctalByteValue(..)
  , HexByteValue(..)
  , LittleUValue(..)
  , BigUValue(..)
  , EscapedChar(..)
  , StringLit(..)
  , RawStringLit(..)
  , InterpretedStringLit(..)
  , TypeName(..)
  , IdentifierList(..)
  , FunctionName(..)
  , BasicLit(..)
  , OperandName(..)
  , QualifiedIdent(..)
  , FieldName(..)
  , Selector(..)
  , BinaryOp(..)
  , RelOp(..)
  , AddOp(..)
  , MulOp(..)
  , UnaryOp(..)
  , PackageClause(..)
  , PackageName(..)
  , EmptyStmt(..)
  , Label(..)
  , AssignOp(..)
  , FallthroughStmt(..)
  , ImportDecl(..)
  , ImportSpec(..)
  , ImportPath(..)
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

data Identifier
  = Identifier Letter [Either Letter UnicodeDigit]
  deriving (Eq, Show)

data IntLit
  = IntLit_DecimalLit DecimalLit
  | IntLit_OctalLit OctalLit
  | IntLit_HexLit HexLit
  deriving (Eq, Show)

data DecimalLit
  = DecimalLit NonZeroDecimalDigit [DecimalDigit]
  deriving (Eq, Show)

data NonZeroDecimalDigit
  = NonZeroDecimalDigit_1
  | NonZeroDecimalDigit_2
  | NonZeroDecimalDigit_3
  | NonZeroDecimalDigit_4
  | NonZeroDecimalDigit_5
  | NonZeroDecimalDigit_6
  | NonZeroDecimalDigit_7
  | NonZeroDecimalDigit_8
  | NonZeroDecimalDigit_9
  deriving (Eq, Show)

newtype OctalLit
  = OctalLit [OctalDigit]
  deriving (Eq, Show)

data HexLit
  = HexLit X HexDigit [HexDigit]
  deriving (Eq, Show)

data X
  = X_Upper
  | X_Lower
  deriving (Eq, Show)

data FloatLit
  = FloatLit_Trailing Decimals (Maybe Decimals) (Maybe Exponent)
  | FloatLit_Exponent Decimals Exponent
  | FloatLit_Leading Decimals (Maybe Exponent)
  deriving (Eq, Show)

data Decimals
  = Decimals DecimalDigit [DecimalDigit]
  deriving (Eq, Show)

data Exponent
  = Exponent E (Maybe Sign) Decimals
  deriving (Eq, Show)

data E
  = E_Upper
  | E_Lower
  deriving (Eq, Show)

data Sign
  = Sign_Positive
  | Sign_Negative
  deriving (Eq, Show)

data ImaginaryLit
  = ImaginaryLit_Decimals Decimals
  | ImaginaryLit_FloatLit FloatLit
  deriving (Eq, Show)

data RuneLit
  = RuneLit_UnicodeValue UnicodeValue
  | RuneLit_ByteValue ByteValue
  deriving (Eq, Show)

data UnicodeValue
  = UnicodeValue_UnicodeChar UnicodeChar
  | UnicodeValue_LittleUValue LittleUValue
  | UnicodeValue_BigUValue BigUValue
  | UnicodeValue_EscapedChar EscapedChar
  deriving (Eq, Show)

data ByteValue
  = ByteValue_OctalByteValue OctalByteValue
  | ByteValue_HexByteValue HexByteValue
  deriving (Eq, Show)

data OctalByteValue
  = OctalByteValue OctalDigit OctalDigit OctalDigit
  deriving (Eq, Show)

data HexByteValue
  = HexByteValue HexDigit HexDigit
  deriving (Eq, Show)

data LittleUValue
  = LittleUValue HexDigit HexDigit HexDigit HexDigit
  deriving (Eq, Show)

data BigUValue
  = BigUValue HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit HexDigit
  deriving (Eq, Show)

data EscapedChar
  = EscapedChar_Bell
  | EscapedChar_Backspace
  | EscapedChar_FormFeed
  | EscapedChar_LineFeed
  | EscapedChar_CarriageReturn
  | EscapedChar_HorizontalTab
  | EscapedChar_VerticalTab
  | EscapedChar_Backslash
  | EscapedChar_SingleQuote
  | EscapedChar_DoubleQuote
  deriving (Eq, Show)

data StringLit
  = StringLit_RawStringLit RawStringLit
  | StringLit_InterpretedStringLit InterpretedStringLit
  deriving (Eq, Show)

newtype RawStringLit
  = RawStringLit [Either UnicodeChar Newline]
  deriving (Eq, Show)

newtype InterpretedStringLit
  = InterpretedStringLit [Either UnicodeValue ByteValue]
  deriving (Eq, Show)

data TypeName
  = TypeName_Unqualified Identifier
  | TypeName_Qualified QualifiedIdent
  deriving (Eq, Show)

data IdentifierList
  = IdentifierList Identifier [Identifier]
  deriving (Eq, Show)

newtype FunctionName
  = FunctionName Identifier
  deriving (Eq, Show)

data BasicLit
  = BasicLit_IntLit IntLit
  | BasicLit_FloatLit FloatLit
  | BasicLit_ImaginaryLit ImaginaryLit
  | BasicLit_RuneLit RuneLit
  | BasicLit_StringLit StringLit
  deriving (Eq, Show)

data OperandName
  = OperandName_Unqualified Identifier
  | OperandName_Qualified QualifiedIdent
  deriving (Eq, Show)

data QualifiedIdent
  = QualifiedIdent PackageName Identifier
  deriving (Eq, Show)

newtype FieldName
  = FieldName Identifier
  deriving (Eq, Show)

newtype Selector
  = Selector Identifier
  deriving (Eq, Show)

data BinaryOp
  = BinaryOp_ConditionalOr
  | BinaryOp_ConditionalAnd
  | BinaryOp_RelOp RelOp
  | BinaryOp_AddOp AddOp
  | BinaryOp_MulOp MulOp
  deriving (Eq, Show)

data RelOp
  = RelOp_Equal
  | RelOp_NotEqual
  | RelOp_Less
  | RelOp_LessOrEqual
  | RelOp_Greater
  | RelOp_GreaterOrEqual
  deriving (Eq, Show)

data AddOp
  = AddOp_Sum
  | AddOp_Difference
  | AddOp_BitwiseOr
  | AddOp_BitwiseXor
  deriving (Eq, Show)

data MulOp
  = MulOp_Product
  | MulOp_Quotient
  | MulOp_Remainder
  | MulOp_LeftShift
  | MulOp_RightShift
  | MulOp_BitwiseAnd
  | MulOp_BitClear
  deriving (Eq, Show)

data UnaryOp
  = UnaryOp_Positive
  | UnaryOp_Negation
  | UnaryOp_Not
  | UnaryOp_BitwiseComplement
  | UnaryOp_Indirection
  | UnaryOp_Address
  | UnaryOp_Receive
  deriving (Eq, Show)

newtype PackageClause
  = PackageClause PackageName
  deriving (Eq, Show)

newtype PackageName
  = PackageName Identifier
  deriving (Eq, Show)

data EmptyStmt
  = EmptyStmt
  deriving (Eq, Show)

newtype Label
  = Label Identifier
  deriving (Eq, Show)

data AssignOp
  = AssignOp_Normal
  | AssignOp_AddOp AddOp
  | AssignOp_MulOp MulOp
  deriving (Eq, Show)

data FallthroughStmt
  = FallthroughStmt
  deriving (Eq, Show)

data ImportDecl
  = ImportDecl_One ImportSpec
  | ImportDecl_Many [ImportSpec]
  deriving (Eq, Show)

data ImportSpec
  = ImportSpec_Unqualified ImportPath
  | ImportSpec_Explicit PackageName ImportPath
  | ImportSpec_Implicit ImportPath
  deriving (Eq, Show)

newtype ImportPath
  = ImportPath StringLit
  deriving (Eq, Show)
