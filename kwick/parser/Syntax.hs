-- Everything in this module is exported
module Syntax where

newtype UnresolvedIdent = UnresolvedIdent [String] deriving (Show)

newtype LocalIdent = LocalIdent String deriving (Show)

data ImportTermination = ImportSome [LocalIdent] | ImportAll deriving (Show)
data ImportPath = ImportPath [LocalIdent] ImportTermination deriving (Show)

data Type
	= OpaqueType UnresolvedIdent
	| TemplateParameterType LocalIdent
	| TemplateType UnresolvedIdent [Type]
	| ReferenceType Type
	| NullableType Type
	| PointerType Type
	| FunctionType [ArgumentDefInterface] [Type]
	deriving (Show)

data ArgumentPassMode = NamedArg | PositionalArg deriving (Show)
data ArgumentStaticMode = RuntimeArg | StaticArg deriving (Show)
data ArgumentDefInterface =
	ArgumentDefInterface ArgumentStaticMode (Maybe LocalIdent) Type
	deriving (Show)
data ArgumentDef
	= RuntimeArgumentDef ArgumentPassMode LocalIdent Type
	| StaticArgumentDef (Maybe LocalIdent) Type
	deriving (Show)
data LambdaArgumentDef = LambdaArgumentDef ArgumentPassMode LocalIdent (Maybe Type) deriving (Show)

data Access = Public | Private deriving (Show)
data GetterAccess = PublicGetter | PrivateGetter deriving (Show)
data SetterAccess = PublicSetter | PrivateSetter deriving (Show)
data ConstructorAccess = PublicConstructor | PrivateConstructor deriving (Show)
data ExtensionAccess = PublicExtension | PrivateExtension deriving (Show)
data ClassAccess = PublicClass ConstructorAccess ExtensionAccess | PrivateClass deriving (Show)
data StructCaseAccess = PublicCase ConstructorAccess | PrivateCase deriving (Show)

data FieldContent = FieldInitializer Expr | FieldType Type deriving (Show)
data Field = Field GetterAccess SetterAccess BindMode LocalIdent FieldContent deriving (Show)

data StructMode = ValueStruct | RefStruct deriving (Show)
data StructCase = StructCase StructCaseAccess [Field] [(LocalIdent, StructCase)] deriving (Show)

data SetterMode = DestructiveSetter | ConstructiveSetter deriving (Show)

data SpecialArgument = SpecialArgument LocalIdent Type deriving (Show)

data ProtocolRequirement
	= FuncRequirement UnresolvedIdent [ArgumentDefInterface] [Type]
	| GetterRequirement UnresolvedIdent Type [ArgumentDefInterface] Type
	| SetterRequirement (Maybe SetterMode) UnresolvedIdent Type [ArgumentDefInterface] Type
	deriving (Show)

data OpenType
	= OpenFunc
	| OpenGetter
	| OpenSetter
	deriving (Show)

data Dec
	= FuncDec Access UnresolvedIdent [ArgumentDef] [Type] [Stat]
	| StructDec StructMode LocalIdent StructCase
	| GetterDec
		Access
		UnresolvedIdent
		SpecialArgument
		[ArgumentDef]
		Type
		[Stat]
	| SetterDec
		Access
		SetterMode
		UnresolvedIdent
		SpecialArgument
		[ArgumentDef]
		SpecialArgument
		[Stat]
	| MethodDec Access UnresolvedIdent ArgumentDef [ArgumentDef] [Type] [Stat]
	| ProtocolDec Access LocalIdent [LocalIdent]  [ProtocolRequirement]
	| OpenDec OpenType LocalIdent
	deriving (Show)

data BindMode = VarBinding | LetBinding deriving (Show)

data ExtractTarget = ExtractToType Type | ExtractFromNullable deriving (Show)

data ExtractClause = ExtractClause BindMode LocalIdent (Maybe Expr) ExtractTarget [Stat]
	deriving (Show)

data Stat
	= BindStat BindMode LocalIdent Expr
	| NewBindStat LocalIdent Type
	| AssignStat [Expr] [Expr]
	| CallStat Expr [Argument]
	| BlockStat [Stat]
	| IfStat [(Expr, [Stat])] (Maybe [Stat])
	| LoopStat [Stat]
	| WhileLoopStat Expr [Stat]
	| ForLoopStat LocalIdent Expr [Stat]
	| ExtractStat [ExtractClause]
	| WhileStat Expr
	| BreakStat
	| ContinueStat
	| ReturnStat [Expr]
	| ValueStat [Expr]
	deriving (Show)

data Expr
	= BindingExpr UnresolvedIdent
	| RefExpr Expr
	| AddressOfExpr Expr
	| DereferenceExpr Expr
	| CallExpr Expr [Argument]
	| AccessorExpr Expr UnresolvedIdent [Argument]
	| IntLitExpr Integer
	| RealLitExpr Double
	| StringLitExpr String
	| CastExpr Expr Type
	| StatExpr Stat
	| LambdaExpr [LambdaArgumentDef] (Maybe [Type]) [Stat]
	deriving (Show)

data Argument
	= RuntimeArgument (Maybe LocalIdent) Expr
	| StaticArgument (Maybe LocalIdent) Type
	deriving (Show)

data Module = Module [ImportPath] [Dec] deriving (Show)
