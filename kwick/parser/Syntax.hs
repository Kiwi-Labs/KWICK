module Syntax
	(UnresolvedIdent (..)
	,LocalIdent (..)
	,Type (..)
	,ArgumentMode (..)
	,ArgumentDefInterface (..)
	,ArgumentDef (..)
	,Dec (..)
	,BindMode (..)
	,Stat (..)
	,Expr (..)
	,Argument (..))
where

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

data Dec
	= FuncDec UnresolvedIdent [ArgumentDef] [Type] [Stat]
	deriving (Show)

data BindMode = VarBinding | LetBinding deriving (Show)

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
	| WhileStat Expr
	| BreakStat
	| ContinueStat
	| ReturnStat [Expr]
	| ValueStat [Expr]
	deriving (Show)

data Expr
	= BindingExpr UnresolvedIdent -- √
	| RefExpr Expr
	| AddressOfExpr Expr
	| DereferenceExpr Expr
	| CallExpr Expr [Argument]
	| AccessorExpr Expr UnresolvedIdent [Argument]
	| IntLitExpr Integer -- √
	| RealLitExpr Double -- √
	| StringLitExpr String -- √
	| CastExpr Expr Type
	| StatExpr Stat
	deriving (Show)

data Argument
	= Argument (Maybe LocalIdent) Expr
	| StaticArgument (Maybe LocalIdent) Type -- NOT YET SUPPORTED
	deriving (Show)
