module Main where

import Parse
import ParseModule
import Control.Monad (forever)
import System.Environment (getArgs)

main = do
	args <- getArgs
	case args of
		[] -> forever $ getLine >>= (print . runParse parseModule)
		[filename] -> readFile filename >>= (print . runParse parseModule)
		_ -> putStrLn "Wrong number of command-line arguments!"
