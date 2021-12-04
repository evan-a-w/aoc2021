module P4 where

import Data.Function
import Data.List
import Debug.Trace

split :: Eq a => a -> [a] -> [[a]]
split = go []
  where
    go acc _ []
      | null acc = []
      | otherwise = [reverse acc]
    go acc s (x:xs)
      | x == s =
        case acc of
          [] -> go [] s xs
          _ -> reverse acc : go [] s xs
      | otherwise = go (x : acc) s xs

p4p1 :: IO ()
p4p1 = do
  inp <- readFile "inputs/p4p1.txt"
  let ls = lines inp
      calls = map read . split ',' . head $ ls :: [Int]
      -- tables is list of 2d lists representing the numbers
      tables =
        map (map (map read . split ' ')) . split "" . tail $ ls :: [[[Int]]]
      c l b = any (all (`elem` l)) $ b ++ transpose b
      final =
        fmap head . (\x -> (x, filter (c (take x calls)) tables)) $
        until (\x -> any (c (take x calls)) tables) ((+) 1) 1 :: (Int, [[Int]])
      score =
        (*)
          (sum .
           filter (not . flip elem (take (fst final) calls)) . foldr (++) [] $
           snd final)
          (head . reverse . take (fst final) $ calls)
  putStrLn $ show score

p4p2 :: IO ()
p4p2 = do
  inp <- readFile "inputs/p4p1.txt"
  let ls = lines inp
      calls = map read . split ',' . head $ ls :: [Int]
      -- tables is list of 2d lists representing the numbers
      tables =
        map (map (map read . split ' ')) . split "" . tail $ ls :: [[[Int]]]
      c l b = any (all (`elem` l)) $ b ++ transpose b
      final =
        fmap head . (\x -> (x, filter (not . c (take (x - 1) calls)) tables)) $
        until (\x -> all (c (take x calls)) tables) ((+) 1) 1 :: (Int, [[Int]])
      score =
        let fcalls = take (fst final) calls
         in (*)
              (sum . filter (not . flip elem fcalls) . foldr (++) [] $ snd final)
              (head . reverse $ fcalls)
  putStrLn $ show score

sol4 = [p4p1, p4p2]
