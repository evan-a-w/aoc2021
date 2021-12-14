module P14 where

import Data.Function
import Data.List
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import P5 (splitS)

p14p1 :: IO ()
p14p1 = do
  inp <- readFile "inputs/p14.txt"
  let lns = lines inp
      rules =
        map (((\[[a:b:[]], [c:[]]] -> ((a, b), c)) . map words) . splitS "->") .
        drop 2 $
        lns
      step = go rules
      go (((a, b), c):rest) (x:y:ys)
        | x == a && y == b = (x : c : (step (y : ys)))
        | otherwise =
          if null rest
            then x : step (y : ys)
            else go rest (x : y : ys)
      go _ [y] = [y]
      go _ [] = []
      tenthPoly = iterate (go rules) (head lns) !! 10
      counts = map length . group . sort $ tenthPoly
      res = maximum counts - minimum counts
  putStrLn . show $ res

type Polymer = Map Char (Map (Maybe Char) Int)

p14p2 :: IO ()
p14p2 = do
  inp <- readFile "inputs/test.txt"
  let lns = lines inp
      rules =
        map (((\[[a:b:[]], [c:[]]] -> ((a, b), c)) . map words) . splitS "->") .
        drop 2 $
        lns
      f :: String -> Polymer -> Polymer
      f (x:xs) m =
        f xs $
        Map.insertWith
          (Map.unionWith (+))
          x
          (Map.fromList
             [ ( if null xs
                   then Nothing
                   else Just . head $ xs
               , 1)
             ])
          m
      f [] m = m
      init = f (head lns) Map.empty
      g' :: ((Char, Char), Char) -> Polymer -> Polymer -> Polymer
      g' ((a, b), c) m tm =
        let newMap = case Map.lookup a m of
          Nothing -> m
          Just m' ->
            case Map.lookup (Just b) m' of
              Nothing -> m
              Just n ->
                let m'' = Map.insertWith (+) (Just c) 1 m'
                    new =
                      if n > 1
                        then Map.insert (Just b) (n - 1) m''
                        else Map.delete (Just b) m''
                    na = Map.insert a new m
                 in Map.insertWith
                      (Map.unionWith (+))
                      c
                      (Map.fromList [(Just b, 1)])
                      na
      g (x:xs) m tm = if null xs then g' x m tm else g xs m (g' x m tm)
      step m = g rules m m
      m = iterate step init !! 40
      lm =
        map snd $ sortBy (compare `on` snd) $ Map.assocs (fmap (foldr (+) 0) m)
  putStrLn . show $ step init
  putStrLn . show $ last lm - head lm

sol14 = [p14p1, p14p2]
