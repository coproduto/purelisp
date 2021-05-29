module Main where

import System.Environment (getArgs)
import Text.Parsec (parse)

import Lib

main :: IO ()
main = do
  args <- getArgs
  if null args
    then putStrLn "purelisp: No input. Exiting."
    else let input = head args
         in print (parse parseLispInput "<stdio>" input)
