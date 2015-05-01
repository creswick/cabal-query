module Main where

import System.Environment

import Distribution.Query

main :: IO ()
main = do
  args <- getArgs
  let file = args!!0
  loadDescr file
