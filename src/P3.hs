module P3 where

import Data.Function
import Data.List
import Debug.Trace

-- from http://pleac.sourceforge.net/pleac_haskell/numbers.html
bin2dec :: String -> Integer
bin2dec = foldr (\c s -> s * 2 + c) 0 . reverse . map c2i
  where
    c2i c =
      if c == '0'
        then 0
        else 1

p3p1 :: IO ()
p3p1 = do
  inp <- readFile "inputs/p3p1.txt"
  let sortByLen = sortBy (compare `on` length)
      ms = map (head . head . sortByLen . group . sort) $ transpose $ lines inp
      opp x
        | x == '0' = '1'
        | otherwise = '0'
      ls = map opp ms
  putStrLn $ show $ (bin2dec ms) * (bin2dec ls)

p3p2 :: IO ()
p3p2 = do
  inp <- readFile "inputs/p3p1.txt"
  let lins = lines inp
      sortByLen = sortBy (compare `on` (\x -> -(length x)))
      ms :: Int -> ([String] -> Char)
      ms i =
        (\x ->
           let lx = (length . head) x
            in head . maximum . takeWhile (\y -> lx == length y) $ x) .
        (sortByLen . group . sort . (!! i) . transpose)
      opp x
        | x == '0' = '1'
        | otherwise = '0'
      filt :: Bool -> [String] -> Int -> String
      filt most lin n
        | length lin == 1 = head lin
        | otherwise =
          let a =
                if most
                  then (ms n) lin
                  else (opp . ms n) lin
              nlin = filter (\x -> x !! n == a) lin
           in filt most nlin (n + 1)
      ox = filt True lins 0
      c02 = filt False lins 0
  putStrLn . show $ (bin2dec ox) * (bin2dec c02)

sol3 :: [IO ()]
sol3 = [p3p1, p3p2]
