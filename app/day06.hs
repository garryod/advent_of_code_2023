module Main where

import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

data Race = Race {duration :: Int, record :: Int} deriving (Show)

main :: IO ()
main = do
  handle <- openFile "data/day06.txt" ReadMode
  contents <- hGetContents handle
  print (product $ map (length . winningHoldDurations) (readRaces contents))
  hClose handle

winningHoldDurations :: Race -> [Int]
winningHoldDurations race = filter recordBreaking [0 .. duration race]
  where
    recordBreaking holdDuration = distanceTravelled (duration race) holdDuration > record race

distanceTravelled :: Int -> Int -> Int
distanceTravelled raceDuration holdDuration = holdDuration * (raceDuration - holdDuration)

readRaces :: String -> [Race]
readRaces input = zipWith (curry intoRace) times distances
  where
    [timesRow, distancesRow] = take 2 $ lines input
    (_ : timesWords) = words timesRow
    (_ : distancesWords) = words distancesRow
    times = map read timesWords
    distances = map read distancesWords
    intoRace (time, distance) = Race time distance
