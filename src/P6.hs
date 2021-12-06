module P6 where

import Data.Function
import Data.List
import Data.Map.Strict (Map)
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

type Fish = Map Int Int

p6p2 :: IO ()
p6p2 = do
  inp <- readFile "inputs/p6.txt"
  let initial_list = map read . split ',' $ inp :: [Int]
      initial_fish :: Fish
      initial_fish =
        foldr (\x y -> Map.insertWith (+) x 1 y) Map.empty initial_list
      inc :: Int -> Int -> Fish -> Fish
      inc k num m
        | k == 0 =
          let a = Map.insertWith (+) 8 num m
           in Map.insertWith (+) 6 num a
        | otherwise = Map.insertWith (+) (k - 1) num m
      inc_day (xs, i) = (Map.foldrWithKey inc Map.empty xs, i + 1)
      final_fish = fst $ until (\(_, d) -> d == 256) inc_day (initial_fish, 0)
      tf x =
        foldr (+) 0 . fst $
        until (\(_, d) -> d == 256) inc_day (Map.fromList [(x, 1)], 0)
      xs = map (\x -> (x ^) <$> [0 .. 8]) [0 .. 8]
      letters = map (\x -> [x]) "abcdefgh"
      w = map (\x -> zipWith (++) (show <$> x) letters) xs
  putStrLn . show . foldr (+) 0 $ final_fish

extra = do
  inp <- readFile "inputs/p6.txt"
  let initial_list = map read . split ',' $ inp :: [Int]
      initial_fish :: Fish
      initial_fish =
        foldr (\x y -> Map.insertWith (+) x 1 y) Map.empty initial_list
      inc :: Int -> Int -> Fish -> Fish
      inc k num m
        | k == 0 =
          let a = Map.insertWith (+) 8 num m
           in Map.insertWith (+) 6 num a
        | otherwise = Map.insertWith (+) (k - 1) num m
      inc_day (xs, i) = (Map.foldrWithKey inc Map.empty xs, i + 1)
      final_fish = fst $ until (\(_, d) -> d == 256) inc_day (initial_fish, 0)
      tf x =
        foldr (+) 0 . fst $
        until (\(_, d) -> d == 256) inc_day (Map.fromList [(x, 1)], 0)
      xs = map (\x -> (x ^) <$> [0 .. 8]) [0 .. 8]
  putStr "["
  putStrLn $
    foldl
      (\a b ->
         if a == ""
           then b
           else a ++ "; " ++ b)
      "" $
    show <$> (zipWith (\a b -> a ++ [b]) xs (map tf [0 .. 8]))
  putStrLn "]"

sol6 = [p6p1, p6p2, extra]
