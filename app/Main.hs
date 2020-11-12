module Main where

import           Sriscem
import           Example

main :: IO ()
main = do
  v <- runProg prog01
  print v
