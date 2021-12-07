module P7 where

import Data.Function
import Data.List
import P4 (split)

p7p1 :: IO ()
p7p1 = do
  inp <- readFile "inputs/p7.txt"
  let crabs = map read . split ',' $ inp :: [Int]
      sCrabs = sort crabs
      med =
        case length sCrabs `rem` 2 of
          1 -> sCrabs !! (length sCrabs `div` 2)
          _ ->
            (sCrabs !! (length sCrabs `div` 2) +
             sCrabs !! (length sCrabs `div` 2)) `div`
            2
  putStrLn . show . sum . map (\x -> abs (x - med)) $ crabs

p7p2 :: IO ()
p7p2 = do
  inp <- readFile "inputs/p7.txt"
  let crabs = map read . split ',' $ inp :: [Int]
      cost i =
        sum .
        map
          (\x ->
             let p = abs (x - i)
              in p * (p + 1) `div` 2) $
        crabs
      res = minimum $ cost <$> [minimum crabs .. maximum crabs]
  putStrLn . show $ res

sol7 = [p7p1, p7p2]
