module Main

import Data.Vect

Location : Type
Location = Vect 2 Nat

parseFile : String -> List (List Char)
parseFile = (map unpack) . lines

moveLocationAcrossBoard : Location -> Location -> List (List Char) -> (Location, Bool) 
moveLocationAcrossBoard loc slope board = (newLoc, currChar == strHead "#")
  where
    currChar : Char
    currChar = case inBounds (last loc) board of
      Yes prf => case inBounds (head loc) (index (last loc) board) of
        Yes prf2 => index (head loc) (index (last loc) board)
        No ctf2 => strHead "."
      No ctf => strHead "."
    newLoc : Location
    newLoc = let [x1, y1] = loc 
                 [x2, y2] = slope 
             in  [fromMaybe 9999 $ mod (x1 + x2) <$> length <$> head' board, y1 + y2]

moveLocationLoop : Location -> Location -> List (List Char) -> Nat -> Nat
moveLocationLoop loc slope board num = 
  let (newLoc, returnBool) = moveLocationAcrossBoard loc slope board
  in if last newLoc <= length board 
        then moveLocationLoop newLoc slope board (num + if returnBool == True then 1 else 0) 
        else num

slopes : List Location
slopes = [[1, 1], [3, 1], [5, 1], [7, 1], [1, 2]] 

main : IO ()
main = do
  file <- readFile "input.txt"
  case file of
    Left err => printLn err
    Right content => (printLn $ moveLocationLoop [0, 0] [3, 1] (parseFile content) 0)
                     <* (printLn $ product $ (\x => moveLocationLoop [0, 0] x (parseFile content) 0) <$> slopes)
