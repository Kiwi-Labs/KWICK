-- Everything in this module is exported
module Syntax where

newtype UnresolvedIdent = UnresolvedIdent [String] deriving (Show)

newtype LocalIdent = LocalIdent String deriving (Show)

data Type
	= OpaqueType UnresolvedIdent
	| TemplateParameterType LocalIdent
	| TemplateType UnresolvedIdent [Type]
	| ReferenceType Type
	| NullableType Type
	| PointerType Type
	| FunctionType [ArgumentDefInterface] [Type]
	deriving (Show)

data ArgumentMode = NamedArg | PositionalArg deriving (Show)
data ArgumentDefInterface = ArgumentDefInterface (Maybe LocalIdent) Type deriving (Show)
data ArgumentDef = ArgumentDef ArgumentMode LocalIdent Type deriving (Show)
data LambdaArgumentDef = LambdaArgumentDef ArgumentMode LocalIdent (Maybe Type) deriving (Show)

data Access = Public | Private deriving (Show)
data GetterAccess = PublicGetter | PrivateGetter deriving (Show)
data SetterAccess = PublicSetter | PrivateSetter deriving (Show)
data ConstructorAccess = PublicConstructor | PrivateConstructor deriving (Show)
data ExtensionAccess = PublicExtension | PrivateExtension deriving (Show)
data ClassAccess = PublicClass ConstructorAccess ExtensionAccess | PrivateClass deriving (Show)
data StructCaseAccess = PublicCase ConstructorAccess | PrivateCase deriving (Show)

data FieldContent = FieldInitializer Expr | FieldType Type deriving (Show)
data Field = Field GetterAccess SetterAccess BindMode LocalIdent FieldContent deriving (Show)

data StructCase = StructCase StructCaseAccess [Field] [(LocalIdent, StructCase)] deriving (Show)

data SetterMode = DestructiveSetter | ConstructiveSetter deriving (Show)

data SpecialArgument = SpecialArgument LocalIdent Type deriving (Show)

data Dec
	= FuncDec Access UnresolvedIdent [ArgumentDef] [Type] [Stat]
	| StructDec LocalIdent StructCase
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
	= Argument (Maybe LocalIdent) Expr
	| StaticArgument (Maybe LocalIdent) Type -- NOT YET SUPPORTED
	deriving (Show)
