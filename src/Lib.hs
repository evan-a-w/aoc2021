module Lib where

p1p1 :: IO ()
p1p1 = do
  inp <- readFile "inputs/p1p1.txt"
  let x = (map read) . words $ inp :: [Int]
  putStrLn . show . length . filter (\(a, b) -> b > a) $ zipWith (,) x (tail x)

p1p2 :: IO ()
p1p2 = do
  inp <- readFile "inputs/p1p1.txt"
  let x = (map read) . words $ inp :: [Int]
  putStrLn . show . length . filter
    (\(a, b) -> b > a) $ let threes = zipWith (\(a, b) c -> [a,b,c])
                                      (zipWith (,) x (tail x))
                                      ((tail . tail) x)
                             sums = map sum threes in
                         zipWith (,) sums (tail sums)
