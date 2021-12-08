module P8 where

import Data.Function
import Data.List
import P4 (split)

p8p1 :: IO ()
p8p1 = do
  inp <- readFile "inputs/p8.txt"
  let l =
        sum .
        map
          (length .
           filter (\x -> length x `elem` [2, 3, 4, 7]) .
           words . last . split '|') .
        lines $
        inp
  putStrLn . show $ l

p8p2 :: IO ()
p8p2 = undefined

sol8 = [p8p1, p8p2]
