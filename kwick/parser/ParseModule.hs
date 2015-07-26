module Parser.ParseModule
	(parseModule)
where

import Parser.Parse
import Parser.Syntax
import Parser.ParseSpace
import Parser.ParseDec
import Parser.ParseIdent

parseImportSome :: Parse Char ImportTermination
parseImportSome = fmap ImportSome $ ksingleOrParenthesized parseLocalIdent

parseImportAll :: Parse Char ImportTermination
parseImportAll = lit '*' >> return ImportAll

parseImportTermination :: Parse Char ImportTermination
parseImportTermination = parseEither parseImportSome parseImportAll

parseImportPath :: Parse Char ImportPath
parseImportPath = greedy $ do
	initPath <- many $ do
		name <- parseLocalIdent
		optional kspace
		lits "::"
		optional kspace
		return name
	term <- parseImportTermination
	return $ ImportPath initPath term

parseImport :: Parse Char ImportPath
parseImport = greedy $ do
	lits "import"
	kspace
	path <- parseImportPath
	ksemicolon
	return path

parseModule :: Parse Char Module
parseModule = greedy $ do
	optional kspace
	toImport <- many $ do -- not named "imports" because ST2 highlights that weirdly...
		path <- parseImport
		optional kspace
		return path
	decs <- many $ do
		dec <- parseDec
		optional kspace
		return dec
	return $ Module toImport decs
