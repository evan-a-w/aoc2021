module Main where

import Lib

main :: IO ()
main = sequence_ (doProb <$> [1 .. 4])
