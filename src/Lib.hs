module Lib where

import P1
import P2

doProb :: Int -> IO ()
doProb a = do
  putStrLn $ "Problem " ++ show a ++ ": "
  let sol = case a of
        1 -> sol1
        2 -> sol2
        otherwise -> replicate 2 (putStrLn "Not implemented")
  putStrLn "Part 1:"
  sol !! 0
  putStrLn "Part 2:"
  sol !! 1
