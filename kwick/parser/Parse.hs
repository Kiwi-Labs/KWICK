module Parse
	(ParseError
	,Parse
	,parseFailure
	,consume1
	,endOfSequence
	,parseEither
	,runParse
	,greedy
	
	,choice
	,lit
	,litCond
	,lits
	,litRange
	,many
	,many1
	,greedyMany
	,greedyMany1
	,reluctantMany
	,reluctantMany1
	,optional
	,reluctantOptional
	,delimited
	,delimited1
	,chainNest
	
	,space
	,parseInt
	,parseFloating)
where

import Control.Monad
import Control.Applicative hiding (many, optional)
import Data.Char
import Data.Maybe (fromMaybe)

-- Primitives

newtype ParseError t = ParseError () deriving (Show)
newtype ParseState t = ParseState [t]

data ParseReduction t a
	= FailureReduction (ParseError t)
	| TotalReduction (ParseState t) a
	| ChoiceReduction (ParseState t) (Parse t a) (Parse t a)

newtype Parse t a = Parse (ParseState t -> ParseReduction t a)

parseFailure :: Parse t a
parseFailure = Parse $ \_ -> FailureReduction $ ParseError ()

consume1 :: Parse t t
consume1 = Parse $ \(ParseState tokens) ->
	case tokens of
		(x : xs) -> TotalReduction (ParseState xs) x
		[] -> FailureReduction $ ParseError ()

endOfSequence :: Parse t ()
endOfSequence = Parse $ \(ParseState tokens) ->
	case tokens of
		(_ : _) -> FailureReduction $ ParseError ()
		[] -> TotalReduction (ParseState []) ()

parseEither :: Parse t a -> Parse t a -> Parse t a
parseEither p1 p2 = Parse $ \state -> ChoiceReduction state p1 p2

instance Monad (Parse t) where
	return x = Parse $ \state -> TotalReduction state x
	
	(Parse f1) >>= f2 = Parse $ \state ->
		case f1 state of
			FailureReduction e -> FailureReduction e
			TotalReduction state' result -> let (Parse f2') = f2 result in f2' state'
			ChoiceReduction state' p1 p2 -> ChoiceReduction state' (p1 >>= f2) (p2 >>= f2)

runParse' :: Parse t a -> ParseState t -> Either (ParseError t) (ParseState t, a)
runParse' (Parse f) state =
	case f state of
		FailureReduction e -> Left e
		TotalReduction state' result -> Right (state', result)
		ChoiceReduction state' p1 p2 ->
			case runParse' p1 state' of
				success @ (Right _) -> success
				Left _ -> runParse' p2 state'

runParse :: Parse t a -> [t] -> Either (ParseError t) a
runParse p src =
	let p' = do
		result <- p
		endOfSequence
		return result
	in case runParse' p' (ParseState src) of
		Left e -> Left e
		Right (_, result) -> Right result

greedy :: Parse t a -> Parse t a
greedy p = Parse $ \state ->
	case runParse' p state of
		Left e -> FailureReduction e
		Right (state', result) -> TotalReduction state' result

-- Auxillary instances

instance MonadPlus (Parse t) where
	mzero = parseFailure
	mplus = parseEither

instance Functor (Parse t) where
	fmap = liftM

instance Applicative (Parse t) where
	pure = return
	(<*>) = ap

-- Essential combinators

choice :: [Parse t a] -> Parse t a
choice = msum

lit :: (Eq t) => t -> Parse t t
lit x = do
	token <- consume1
	guard $ token == x
	return x

litCond :: (t -> Bool) -> Parse t t
litCond cond = do
	token <- consume1
	guard $ cond token
	return token

lits :: (Eq t) => [t] -> Parse t [t]
lits l@(x : xs) = do
		token <- consume1
		guard $ token == x
		tokens <- lits xs
		return l
lits [] = return []

litRange :: (Ord t) => (t, t) -> Parse t t
litRange (t1, t2) = do
	token <- consume1
	guard $ token >= t1 && token <= t2
	return token

many :: Parse t a -> Parse t [a]
many p = result where
	parse1 = do
		x <- p
		xs <- result
		return (x : xs)
	result = parseEither parse1 (return [])

many1 :: Parse t a -> Parse t [a]
many1 p = do
	x <- p
	xs <- many p
	return (x : xs)

greedyMany :: Parse t a -> Parse t [a]
greedyMany p = result where
	parse1 = do
		x <- p
		xs <- result
		return (x : xs)
	result = greedy $ parseEither parse1 (return [])

greedyMany1 :: Parse t a -> Parse t [a]
greedyMany1 p = do
	x <- p
	xs <- greedyMany p
	return (x : xs)

reluctantMany :: Parse t a -> Parse t [a]
reluctantMany p = result where
	parse1 = do
		x <- p
		xs <- result
		return (x : xs)
	result = parseEither (return []) parse1

reluctantMany1 :: Parse t a -> Parse t [a]
reluctantMany1 p = do
	x <- p
	xs <- reluctantMany p
	return (x : xs)

optional :: Parse t a -> Parse t (Maybe a)
optional p = parseEither (p >>= (return . Just)) (return Nothing)

reluctantOptional :: Parse t a -> Parse t (Maybe a)
reluctantOptional p = parseEither (return Nothing) (p >>= (return . Just))

delimited :: Parse t a -> Parse t b -> Parse t [b]
delimited d p = parseEither nonEmpty (return []) where
	nonEmpty = do
		x <- p
		xs <- many $ (d >> p)
		return (x : xs)

delimited1 :: Parse t a -> Parse t b -> Parse t [b]
delimited1 d p = do
		x <- p
		xs <- many $ (d >> p)
		return (x : xs)

chainNest :: Parse t a -> [a -> Parse t a] -> Parse t a
chainNest coreP layersP = do
	core <- coreP
	chainNest' core layersP

chainNest' :: a -> [a -> Parse t a] -> Parse t a
chainNest' core layersP = parseEither nest1 (return core) where
	nest1 = do
		core' <- choice $ map ($ core) layersP
		chainNest' core' layersP

-- Useful shorthand and constants

space :: Parse Char [Char]
space = greedyMany1 $ litCond isSpace

parseInt :: (Num n) => Parse Char n
parseInt = (greedyMany1 $ litCond isDigit) >>= (return . fromInteger . read)

parseFloating :: (Floating n, Read n) => Parse Char n
parseFloating = do
	integerPart <- greedyMany1 $ litCond isDigit
	fractionalPart <- optional $ lit '.' >> (greedyMany1 $ litCond isDigit)
	return $ read $ integerPart ++ "." ++ (fromMaybe "" fractionalPart)
