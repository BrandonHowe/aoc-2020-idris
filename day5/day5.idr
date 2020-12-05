module Main

import Data.Vect

boolToNat : Bool -> Nat
boolToNat True  = 1
boolToNat False = 0

padListEnd : (n : Nat) -> a -> (list : List a) -> List a
padListEnd newLen val l = (l <+> (replicate (minus newLen (length l)) val))

parseStr : String -> (Nat, Nat)
parseStr str = (row, col)
  where
    binaryToInt : List Nat -> Nat
    binaryToInt = foldl (\acc, cur => acc * 2 + cur) 0
    row : Nat
    row = binaryToInt $ boolToNat <$> (== strHead "B") <$> (unpack $ (substr 0 7 str))
    col : Nat
    col = binaryToInt $ boolToNat <$> (== strHead "R") <$> (unpack $ (substr (minus (length str) 3) 3 str))

ids : List (Nat, Nat) -> List Nat
ids = map (\x => (fst x * 8) + snd x)

part1 : List (Nat, Nat) -> Nat
part1 list = foldl max 0 $ ids list
    
part2 : List (Nat, Nat) -> Maybe Nat
part2 list = (+1) <$> find (\x => (not $ elem (x + 1) idsOfList) && elem (x + 2) idsOfList) idsOfList
  where
    idsOfList : List Nat
    idsOfList = ids list

main : IO ()
main = do
  file <- readFile "input.txt"
  case file of
    Left err => printLn err
    Right content => (printLn $ part1 $ parseStr <$> lines content)
                  <* (printLn $ part2 $ parseStr <$> lines content)
