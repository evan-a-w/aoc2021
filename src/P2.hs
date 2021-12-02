module P2 where

import Debug.Trace
import Data.List

p2p1 :: IO ()
p2p1 = do
  inp <- readFile "inputs/p2p1.txt"
  putStrLn . show . (foldr (*) 1) $
    foldl' upd [0, 0] (lines inp)
  where upd [h, d] s = let [cmd, n] = words s
                           num      = read n :: Int
                       in
                       case cmd of
                          "forward" -> [h + num, d]
                          "down" -> [h, d + num]
                          "up" -> [h, d - num]
                          otherwise -> error "invalid command"

p2p2 :: IO ()
p2p2 = do
  inp <- readFile "inputs/p2p1.txt"
  putStrLn . show . (foldr (*) 1) . (take 2) $
    foldl' upd [0, 0, 0] (lines inp)
  where upd [h, d, a] s = let [cmd, n] = words s
                              num      = read n :: Int
                          in
                          case cmd of
                             "forward" -> [h + num, d + a * num, a]
                             "down"    -> [h, d, a + num]
                             "up"      -> [h, d, a - num]
                             otherwise -> error "invalid command"

sol2 :: [IO ()]
sol2 = [p2p1, p2p2]
