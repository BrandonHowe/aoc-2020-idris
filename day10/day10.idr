module Main

import Data.String

count : (p : a -> Bool) -> List a -> Nat
count p = length . (filter p)

getGaps : (ls : List Int) -> {auto check : NonEmpty ls} -> List Int
getGaps list = zipWith (\a, b => b - a) list (tail list)

getRuns : Int -> List Int -> List Nat
getRuns delim list = (+1) <$> filter (> 0) (length <$> split (== delim) list)

accumMultiplier : Nat -> Nat -> Nat
accumMultiplier accum num = case toIntNat num of
  2 => accum
  3 => accum * 2
  4 => accum * 4
  5 => accum * 7
  _ => accum

parseLines : String -> List Int
parseLines str = sort $ catMaybes $ parseInteger <$> lines str

part1 : (ls : List Int) -> {auto check : NonEmpty ls} -> Nat
part1 list = count (== 1) gaps * (count (== 3) gaps + 1)
  where
    gaps : List Int
    gaps = getGaps list

part2 : List Nat -> Nat
part2 list = foldl accumMultiplier 1 list

partial
main : IO ()
main = do
  Right content <- readFile "input.txt"
  let things = parseLines content
  printLn $ part1 (0 :: things)
  printLn $ part2 $ getRuns 3 $ getGaps (0 :: things)
