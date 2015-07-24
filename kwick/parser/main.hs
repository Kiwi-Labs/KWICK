module Main where

import Parse
import ParseModule

main = getLine >>= (print . runParse parseModule) >> main
