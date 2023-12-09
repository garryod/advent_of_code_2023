module Main where

import Data.Char (isNumber, isPunctuation)
import System.IO (IOMode (ReadMode), hClose, hGetContents, openFile)

data Location = Location {row :: Int, column :: Int} deriving (Show)

data Component = Component {location :: Location, symbol :: Char} deriving (Show)

data Number = Number {start :: Location, chars :: Int, value :: Int} deriving (Show)

main :: IO ()
main = do
  handle <- openFile "data/day03.txt" ReadMode
  contents <- hGetContents handle
  let componentNumbers = map (associate $ findNumbers contents) (findComponents contents)
  let partNumberTotal = sum $ map componentValue componentNumbers
  print partNumberTotal
  let gearTotal = sum $ map gearRatio $ filter isGear componentNumbers
  print gearTotal
  hClose handle

componentValue :: (Component, [Number]) -> Int
componentValue (component, numbers) = sum $ map value numbers

isGear :: (Component, [Number]) -> Bool
isGear (component, numbers) = (symbol component == '*') && (length numbers == 2)

gearRatio :: (Component, [Number]) -> Int
gearRatio (component, numbers) = product $ map value numbers

associate :: [Number] -> Component -> (Component, [Number])
associate numbers component = (component, filter (isRelated component) numbers)
  where
    isRelated component number = (row . location) component >= (row . start) number - 1 && (row . location) component <= (row . start) number + 1 && (column . location) component >= (column . start) number - 1 && (column . location) component <= (column . start) number + chars number

findComponents :: String -> [Component]
findComponents input = fst $ foldl findComponents ([], 0) (lines input)
  where
    findComponents (columnsAcc, row) line = (columnsAcc ++ map (intoComponent row) (fst (foldl findComponent ([], 0) line)), row + 1)
    findComponent (columns, current) char
      | isComponent char = (columns ++ [(char, current)], current + 1)
      | otherwise = (columns, current + 1)
    isComponent char = char /= '.' && not (isNumber char)
    intoComponent row (char, col) = Component (Location row col) char

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
