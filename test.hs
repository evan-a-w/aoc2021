main = do
  let xs = map (\x -> (x ^) <$> [0 .. 8]) [0 .. 8]
      letters = map (\x -> [x]) "abcdefgh"
      w = map (\x -> zipWith (++) (show <$> x) letters) xs
  putStrLn . show $ w
