module Main

import Data.String
  
part1 : List Int -> List Int
part1 list = do
  a <- list
  b <- list
  guard $ a + b == 2020
  pure $ a * b

part2 : List Int -> List Int
part2 list = do
  a <- list
  b <- list
  c <- list
  guard $ a + b + c == 2020
  pure $ a * b * c
  
parseInput : String -> List Int
parseInput x = catMaybes $ parseInteger <$> lines x

main : IO ()
main = do
  file <- readFile "input.txt"
  case file of
    Right content => putStrLn $ ("Part 1: " ++ (show $ part1 $ parseInput content)) ++ "\n" ++ ("Part 2: " ++ (show $ part2 $ parseInput content))
    Left err => printLn err
