module Main where

import qualified Data.IntSet as IntSet
import qualified Data.Text as Text
import Data.Text.IO (hGetContents)
import System.IO (IOMode (ReadMode), hClose, openFile)

data Card = Card {selected :: IntSet.IntSet, winning :: IntSet.IntSet} deriving (Show)

main :: IO ()
main = do
  handle <- openFile "data/day04.txt" ReadMode
  contents <- hGetContents handle
  print (sum $ map cardPoints (readCards contents))
  hClose handle

cardPoints :: Card -> Int
cardPoints card
  | numWinners == 0 = 0
  | otherwise = 2 ^ (numWinners - 1)
  where
    numWinners = IntSet.size winningNumbers
    winningNumbers = IntSet.intersection (selected card) (winning card)

readCards :: Text.Text -> [Card]
readCards input = map readCard (Text.lines input)
  where
    readCard input = Card (numbers selectedText) (numbers winningText)
      where
        [idText, gameText] = Text.split (== ':') input
        [winningText, selectedText] = Text.split (== '|') gameText
        numbers :: Text.Text -> IntSet.IntSet
        numbers input = IntSet.fromList $ map (read . Text.unpack) (Text.words input)
