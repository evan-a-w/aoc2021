module P5 where

import Data.Function
import Data.List
import qualified Data.Map.Strict as Map
import Debug.Trace

splitS :: Eq a => [a] -> [a] -> [[a]]
splitS = go []
  where
    go :: Eq a => [a] -> [a] -> [a] -> [[a]]
    go acc _ []
      | null acc = []
      | otherwise = [reverse acc]
    go acc s xs =
      let (a, b) = splitAt (length s) xs
       in if a == s
            then reverse acc : go [] s b
            else go (head xs : acc) s $ tail xs

p5p1 :: IO ()
p5p1 = do
  inp <- readFile "inputs/p5p1.txt"
  let ls = lines inp
      diffTups =
        filter (\[[a, b], [c, d]] -> c == a || d == b) .
        map (map (map read . splitS ",")) . map (splitS "->") $
        ls :: [[[Int]]]
      makeList [[a, b], [c, d]] =
        let x = minimum [a, c]
            y = maximum [a, c]
            w = minimum [b, d]
            z = maximum [b, d]
         in if d == b
              then zipWith (,) [x .. y] (repeat d)
              else zipWith (,) (repeat a) [w .. z]
      tracesh s x = trace (s ++ " " ++ show x) x
      updMapInRange z m =
        foldr (\tup m' -> Map.insertWith (+) tup 1 m') m . makeList $ z
      pointMap = foldr updMapInRange Map.empty diffTups
  putStrLn . show . foldr (\_ b -> b + 1) 0 . Map.filter (> 1) $ pointMap

p5p2 :: IO ()
p5p2 = do
  inp <- readFile "inputs/p5p1.txt"
  let ls = lines inp
      diffTups =
        map (map (map read . splitS ",")) . map (splitS "->") $ ls :: [[[Int]]]
      inBtwn a b =
        if a < b
          then [a .. b]
          else reverse [b .. a]
      zipRep f a b =
        if length a > length b
          then zipWith f a (cycle b)
          else zipWith f (cycle a) b
      makeList [[a, b], [c, d]] = zipRep (,) (inBtwn a c) (inBtwn b d)
      updMapInRange z m =
        foldr (\tup m' -> Map.insertWith (+) tup 1 m') m . makeList $ z
      pointMap = foldr updMapInRange Map.empty diffTups
  putStrLn . show . foldr (\_ b -> b + 1) 0 . Map.filter (> 1) $ pointMap

sol5 = [p5p1, p5p2]
