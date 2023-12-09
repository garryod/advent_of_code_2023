module Main where

import Data.Char (isNumber, isPunctuation)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

data Location = Location {row :: Int, column :: Int} deriving (Show)

data Number = Number {start :: Location, chars :: Int, value :: Int} deriving (Show)

main :: IO ()
main = do
  handle <- openFile "data/day03.txt" ReadMode
  contents <- hGetContents handle
  let partNumbers = filter (isPartNumber $ findComponents contents) (findNumbers contents)
  let total = sum $ map value partNumbers
  print total
  hClose handle

isPartNumber :: [Location] -> Number -> Bool
isPartNumber components number = any (isNeighbour number) components
  where
    isNeighbour number component = row component >= (row . start) number - 1 && row component <= (row . start) number + 1 && column component >= (column . start) number - 1 && column component <= (column . start) number + chars number

findComponents :: String -> [Location]
findComponents input = fst $ foldl findColumns ([], 0) (lines input)
  where
    findColumns (columnsAcc, row) line = (columnsAcc ++ map (Location row) (fst (foldl findColumn ([], 0) line)), row + 1)
    findColumn (columns, current) char
      | isComponent char = (columns ++ [current], current + 1)
      | otherwise = (columns, current + 1)
    isComponent char = char /= '.' && not (isNumber char)

findNumbers :: String -> [Number]
findNumbers input = fst $ foldl findNumbers ([], 0) (map (++ ['.']) (lines input))
  where
    findNumbers (columnsAcc, row) line = (columnsAcc ++ map (intoNumber row) (spans (foldl findNumber ([], [], 0) line)), row + 1)
    findNumber (spans, spanChars, current) char
      | isNumber char = (spans, spanChars ++ [char], current + 1)
      | not (isNumber char) && spanChars /= [] = (spans ++ [(spanChars, current)], [], current + 1)
      | otherwise = (spans, spanChars, current + 1)
    intoNumber row (spanChars, end) = Number (Location row (end - length spanChars)) (length spanChars) (read spanChars)
    spans (spans, _, _) = spans
