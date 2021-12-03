module P1 where

p1p1 :: IO ()
p1p1 = do
  inp <- readFile "inputs/p1p1.txt"
  let x = (map read) . words $ inp :: [Int]
  putStrLn . show . length . filter (\(a, b) -> b > a) $ zipWith (,) x (tail x)

p1p2 :: IO ()
p1p2 = do
  inp <- readFile "inputs/p1p1.txt"
  let x = (map read) . words $ inp :: [Int]
  putStrLn . show . length . filter (\(a, b) -> b > a) $
    let sums = map (\(a,b,c) -> a+b+c) $ zip3 x (tail x) ((tail . tail) x)
    in
    zipWith (,) sums (tail sums)

sol1 :: [IO ()]
sol1 = [p1p1, p1p2]
