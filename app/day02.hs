module Main where

import qualified Data.Text as Text
import Data.Text.IO (hGetContents)
import System.IO (IOMode (ReadMode), hClose, openFile)

data Draw = Draw {red :: Int, green :: Int, blue :: Int} deriving (Show)

data Game = Game {gameId :: Int, draws :: [Draw]} deriving (Show)

main :: IO ()
main = do
  handle <- openFile "data/day02.txt" ReadMode
  contents <- hGetContents handle
  let bag = Draw 12 13 14
  print $ sum $ map gameId (filter (possibleGame bag) (map readGame $ Text.lines contents))
  print $ sum $ map ((power . minimumBag) . readGame) (Text.lines contents)
  hClose handle

power :: Draw -> Int
power (Draw red green blue) = red * green * blue

minimumBag :: Game -> Draw
minimumBag (Game _ draws) = foldr update mempty draws
  where
    update bag draw = bag {red = max (red bag) (red draw), green = max (green bag) (green draw), blue = max (blue bag) (blue draw)}

possibleGame :: Draw -> Game -> Bool
possibleGame bag (Game _ draws) = all (possibleDraw bag) draws
  where
    possibleDraw (Draw br bg bb) (Draw dr dg db) = br >= dr && bg >= dg && bb >= db

instance Semigroup Draw where
  (Draw lr lg lb) <> (Draw rr rg rb) = Draw (lr + rr) (lg + rg) (lb + rb)

instance Monoid Draw where
  mempty = Draw 0 0 0

readGame :: Text.Text -> Game
readGame input = Game {gameId = gameId, draws = draws}
  where
    [gameText, drawsText] = Text.split (== ':') input
    gameId = read . Text.unpack . last $ Text.words gameText
    draws = map draw $ Text.split (== ';') drawsText
    draw drawText = foldr update mempty (Text.split (== ',') drawText)
    update drawText draw = case color of
      "red" -> draw {red = count}
      "green" -> draw {green = count}
      "blue" -> draw {blue = count}
      where
        [countText, colorText] = Text.words drawText
        color = Text.unpack colorText
        count = (read . Text.unpack) countText
