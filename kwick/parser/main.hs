module Main where

import Parse
import ParseExpr
import ParseType
import ParseStat
import ParseDec
import ParseModule

main = getLine >>= (print . runParse parseModule) >> main
