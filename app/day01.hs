module Main where

import Data.Char (digitToInt, isDigit)
import Data.Foldable (asum, find)
import Data.List (isPrefixOf, tails)
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
extractCalibration entry = 10 * fromJust (extractFirst entry) + fromJust (extractLast entry)

extractFirst :: String -> Maybe Int
extractFirst entry = asum (map extractNumber (tails entry))

extractLast :: String -> Maybe Int
extractLast entry = asum (map extractNumber (reverse (tails entry)))

extractNumber :: String -> Maybe Int
extractNumber substr
  | isPrefixOf "0" substr || isPrefixOf "zero" substr = Just 0
  | isPrefixOf "1" substr || isPrefixOf "one" substr = Just 1
  | isPrefixOf "2" substr || isPrefixOf "two" substr = Just 2
  | isPrefixOf "3" substr || isPrefixOf "three" substr = Just 3
  | isPrefixOf "4" substr || isPrefixOf "four" substr = Just 4
  | isPrefixOf "5" substr || isPrefixOf "five" substr = Just 5
  | isPrefixOf "6" substr || isPrefixOf "six" substr = Just 6
  | isPrefixOf "7" substr || isPrefixOf "seven" substr = Just 7
  | isPrefixOf "8" substr || isPrefixOf "eight" substr = Just 8
  | isPrefixOf "9" substr || isPrefixOf "nine" substr = Just 9
  | otherwise = Nothing
