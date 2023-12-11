module Main where

import Data.List (sort, sortBy)
import Data.List.NonEmpty (group)
import Data.Ord (Down (Down), comparing)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

data Card = A | K | Q | T | F9 | F8 | F7 | F6 | F5 | F4 | F3 | F2 | J deriving (Eq, Ord, Enum, Show)

data Hand = Hand {cards :: [Card], bid :: Int} deriving (Eq, Show)

data HandType = FiveOfKind | FourOfKind | FullHouse | ThreeOfKind | TwoPair | OnePair | HighCard deriving (Eq, Ord, Enum, Show)

main :: IO ()
main = do
  handle <- openFile "data/day07.txt" ReadMode
  contents <- hGetContents handle
  print (sum $ zipWith (curry winnings) [1 ..] (sortBy (comparing Down) (readHands contents)))
  hClose handle

winnings :: (Int, Hand) -> Int
winnings (rank, card) = rank * bid card

instance Ord Hand where
  compare l r
    | typeOrd /= EQ = typeOrd
    | otherwise = cardOrd $ zip (cards l) (cards r)
    where
      typeOrd = compare (handType l) (handType r)
      cardOrd ((xl, xr) : xs)
        | xl /= xr = compare xl xr
        | otherwise = cardOrd xs

handType :: Hand -> HandType
handType hand
  | numJokers == 5 || head cardCounts + numJokers == 5 = FiveOfKind
  | head cardCounts + numJokers == 4 = FourOfKind
  | head cardCounts + numJokers == 3 && cardCounts !! 1 == 2 = FullHouse
  | head cardCounts + numJokers == 3 = ThreeOfKind
  | head cardCounts + numJokers == 2 && cardCounts !! 1 == 2 = TwoPair
  | head cardCounts + numJokers == 2 = OnePair
  | otherwise = HighCard
  where
    numJokers = length $ filter (== J) (cards hand)
    cardCounts = sortBy (comparing Data.Ord.Down) (map length $ group $ filter (/= J) $ sort (cards hand))

readCard :: Char -> Card
readCard input
  | input == 'A' = A
  | input == 'K' = K
  | input == 'Q' = Q
  | input == 'J' = J
  | input == 'T' = T
  | input == '9' = F9
  | input == '8' = F8
  | input == '7' = F7
  | input == '6' = F6
  | input == '5' = F5
  | input == '4' = F4
  | input == '3' = F3
  | input == '2' = F2

readHands :: String -> [Hand]
readHands input = map readHand $ lines input
  where
    readHand input = Hand cards bid
      where
        [cardsText, bidText] = words input
        cards = map readCard cardsText
        bid = read bidText
