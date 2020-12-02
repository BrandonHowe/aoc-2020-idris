module Main

import Data.String

record Policy where
  constructor MkPolicy
  min : Nat
  max : Nat
  letter : Char

Show Policy where
  show (MkPolicy min max letter) = show letter ++ ": Min: " ++ show min ++ " Max: " ++ show max
  show _ = "Policy"

infixr 10 =<<

extractMaybePair : Pair (Maybe Nat) (Maybe Nat) -> Maybe (Pair Nat Nat)
extractMaybePair pair = let (first, second) = pair in 
  case first of
    Just fVal => case second of
      Just sVal => Just $ (fVal, sVal)
      Nothing => Nothing
    Nothing => Nothing

injectMaybePair : Maybe (Pair a a) -> Pair (Maybe a) (Maybe a)
injectMaybePair pair = case pair of
  (Just p) => let (first, second) = p in (Just first, Just second)
  Nothing => (Nothing, Nothing)

parseLines : List String -> List (Maybe (Policy, String))
parseLines l = parseStr <$> l 
  where
    getRange : String -> Maybe (Nat, Nat)
    getRange str = let (first, second) = break ((==) (strHead "-")) str in extractMaybePair $ (parsePositive first, parsePositive (assert_total $ strTail second))
    getLetter : String -> Maybe Char
    getLetter = head' . unpack
    createPolicy : List String -> Maybe Policy
    createPolicy list = let (min, max) = injectMaybePair firstVal in MkPolicy <$> min <*> max <*> letter 
      where
        firstVal : Maybe (Nat, Nat)
        firstVal = (index' 0 list) >>= getRange
        letter : Maybe Char
        letter = (index' 1 list) >>= getLetter
    parseStr : String -> Maybe (Policy, String)
    parseStr str = MkPair <$> createPolicy splat <*> index' 2 splat
      where
        splat : List String
        splat = words str


count : (a -> Bool) -> List a -> Nat
count p = length . (filter p)

policyMatches : (Policy, String) -> Bool
policyMatches cur = let x = count ((==) l) $ unpack xs in x >= minimum && x <= maximum
  where
    policy : Policy
    policy = fst cur
    xs : String
    xs = snd cur
    minimum : Nat
    minimum = min policy
    maximum : Nat
    maximum = max policy
    l : Char
    l = letter policy

countMatchesTotal : List (Policy, String) -> Nat
countMatchesTotal list = foldl (\acc, cur => if policyMatches cur then acc + 1 else acc) 0 list

part2Valid : (Policy, String) -> Bool
part2Valid cur = ((strIndex xs first) == l) /= ((strIndex xs sndIdx) == l)
  where
    policy : Policy
    policy = fst cur
    xs : String
    xs = snd cur
    first : Int
    first = (toIntNat $ min policy) - 1
    sndIdx : Int
    sndIdx = (toIntNat $ max policy) - 1
    l : Char
    l = letter policy

main : IO ()
main = do
  file <- readFile "input.txt"
  case file of
       Left err => printLn err
       Right content => (printLn $ countMatchesTotal $ catMaybes $ parseLines $ lines content)
                        <* (printLn $ count part2Valid $ catMaybes $ parseLines $ lines content)
