module Main where

import Parse
import ParseExpr
import ParseType

main = getLine >>= (print . runParse parseExpr) >> main
