module Main where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (find)
import Data.Maybe (fromJust)
import System.Environment (getArgs)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

main :: IO ()
main = do
  handle <- openFile "data/day01.txt" ReadMode
  contents <- hGetContents handle
  print (sum (map extractCalibration (lines contents)))
  hClose handle

extractCalibration :: String -> Int
extractCalibration entry = 10 * extractFirst entry + extractLast entry

extractFirst :: String -> Int
extractFirst entry = digitToInt (fromJust (find isDigit entry))

extractLast :: String -> Int
extractLast entry = digitToInt (fromJust (find isDigit (reverse entry)))
