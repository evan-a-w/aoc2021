module Lib where

import P1
import P14
import P2
import P3
import P4
import P5
import P6
import P7
import P8

doProb :: Int -> IO ()
doProb a = do
  putStrLn $ "Problem " ++ show a ++ ": "
  let sol =
        case a of
          1 -> sol1
          2 -> sol2
          3 -> sol3
          4 -> sol4
          5 -> sol5
          6 -> sol6
          7 -> sol7
          8 -> sol8
          14 -> sol14
          otherwise -> replicate 2 (putStrLn "Not implemented")
  putStrLn "Part 1:"
  sol !! 0
  putStrLn "Part 2:"
  sol !! 1
