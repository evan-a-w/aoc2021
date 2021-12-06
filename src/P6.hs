module P6 where

import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Debug.Trace
import P4 (split)

p6p1 :: IO ()
p6p1 = do
  inp <- readFile "inputs/p6.txt"
  let initial_fish = map read . split ',' $ inp :: [Int]
      inc num cl =
        if num == 0
          then 8 : 6 : cl
          else num - 1 : cl
      inc_day (xs, i) = (reverse . foldr inc [] $ xs, i + 1)
      end_fish = fst $ until (\(_, d) -> d == 80) inc_day (initial_fish, 0)
  putStrLn . show . length $ end_fish

p6p2 :: IO ()
p6p2 = do
  inp <- readFile "inputs/test.txt"
  let initial_fish = map read . split ',' $ inp :: [Int]
      inc cl num =
        if num == 0
          then 8 : 6 : cl
          else num - 1 : cl
      inc_day = inc []
      changes_2_to_n n
        | n == 0 = map inc_day
        | otherwise = 
      eight_changes =
        map
          (fst . until (\(_, d) -> d == 8) inc_day . (\x -> ([x], 0)))
          [0 .. 8]
      inc_eight (xs, i) = (concat . map (\x -> eight_changes !! x) $ xs, i + 8)
      change256 =
        map
          (fst . until (\(_, d) -> d == 256) inc_eight . (\x -> ([x], 0)))
          [0 .. 8]
      inc_256 = concat . map (\x -> change256 !! x)
      end_fish = inc_256 initial_fish
  putStrLn . show . length $ end_fish

sol6 = [p6p1, p6p2]
