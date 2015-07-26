{-# LANGUAGE ExistentialQuantification, ScopedTypeVariables #-}

module Parser.Precedence
	(Expr (..)
	,Grouping (..)
	,precedenceGroup)
where

data Expr t o = Terminal t | Binop (Expr t o) o (Expr t o) deriving (Show)

data Grouping = GroupLeft | GroupRight deriving (Show)

precedenceGroup :: forall t o. (o -> o -> Grouping) -> t -> [(o, t)] -> Expr t o
precedenceGroup _ x [] = Terminal x
precedenceGroup comp x ((op, y) : rest) = let
	regroup :: Expr t o -> Expr t o
	regroup (Terminal t) = Binop (Terminal x) op (Terminal t)
	regroup (Binop a op2 b) = case comp op op2 of
		GroupLeft  -> Binop (regroup a) op2 b
		GroupRight -> Binop (Terminal x) op (Binop a op2 b)
	in regroup $ precedenceGroup comp y rest
