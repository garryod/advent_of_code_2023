module Main where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

main :: IO ()
main = do
  filename <- getArgs
  handle <- openFile "data/day01.txt" ReadMode
  contents <- hGetContents handle
  let entries = lines contents
  let calibrations = map extractCalibration entries
  print (sum calibrations)
  hClose handle

extractCalibration :: String -> Int
extractCalibration entry = read [extractFirst entry, extractLast entry]

extractFirst :: String -> Char
extractFirst entry = fromJust (find isDigit entry)

extractLast :: String -> Char
extractLast entry = fromJust (find isDigit (reverse entry))
