module Main where

import qualified Data.Text as Text
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

data Draw = Draw {red :: Int, green :: Int, blue :: Int} deriving (Show)

data Game = Game {gameId :: Int, draws :: [Draw]} deriving (Show)

main :: IO ()
main = do
  handle <- openFile "data/day02.txt" ReadMode
  contents <- hGetContents handle
  let bag = Draw 12 13 14
  print $ sum $ map gameId (filter (possibleGame bag) (map readGame $ lines contents))
  hClose handle

possibleGame :: Draw -> Game -> Bool
possibleGame bag (Game _ draws) = all (possibleDraw bag) draws
  where
    possibleDraw (Draw br bg bb) (Draw dr dg db) = br >= dr && bg >= dg && bb >= db

instance Semigroup Draw where
  (Draw lr lg lb) <> (Draw rr rg rb) = Draw (lr + rr) (lg + rg) (lb + rb)

instance Monoid Draw where
  mempty = Draw 0 0 0

readGame :: String -> Game
readGame input = Game {gameId = gameId, draws = draws}
  where
    [gameText, drawsText] = Text.split (== ':') (Text.pack input)
    gameId = read . Text.unpack . last $ Text.words gameText
    draws = map draw $ Text.split (== ';') drawsText
    draw drawText = foldl update mempty (Text.split (== ',') drawText)
    update draw drawText = case color of
      "red" -> draw {red = count}
      "green" -> draw {green = count}
      "blue" -> draw {blue = count}
      where
        [countText, colorText] = Text.words drawText
        color = Text.unpack colorText
        count = (read . Text.unpack) countText
