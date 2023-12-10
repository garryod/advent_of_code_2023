module Main where

import qualified Data.Text as Text
import Data.Text.IO (hGetContents)
import System.IO (IOMode (ReadMode), hClose, openFile)

data MappedRange t s = MappedRange {target :: t, source :: s, entries :: Int}

type RangeMap t s = [MappedRange t s]

main :: IO ()
main = do
  handle <- openFile "data/day05.txt" ReadMode
  contents <- hGetContents handle
  let (seedText : mapsText) = Text.splitOn (Text.pack "\n\n") contents
  print (minimum $ map (followMaps (readMaps mapsText)) (readSeeds seedText))
  print (minimum $ map (followMaps (readMaps mapsText)) (readSeedRanges seedText))
  hClose handle

followMaps :: [RangeMap Int Int] -> Int -> Int
followMaps maps seed = foldl lookup seed maps
  where
    lookup :: Int -> RangeMap Int Int -> Int
    lookup value mappedRanges = maybe value dereference (find inRange mappedRanges)
      where
        inRange mappedRange = value >= source mappedRange && value < source mappedRange + entries mappedRange
        dereference mappedRange = target mappedRange + value - source mappedRange

readSeeds :: Text.Text -> [Int]
readSeeds input = map (read . Text.unpack) $ Text.words seedsText
  where
    [_, seedsText] = Text.split (== ':') input

readSeedRanges :: Text.Text -> [Int]
readSeedRanges input = concatMap rangeSeeds (pairs $ Text.words seedsText)
  where
    [_, seedsText] = Text.split (== ':') input
    pairs [] = []
    pairs elements = toTuple (take 2 elements) : pairs (drop 2 elements)
    toTuple [a, b] = (a, b)
    rangeSeeds (startSeed, numSeeds) = [read $ Text.unpack startSeed .. read (Text.unpack startSeed) + read (Text.unpack numSeeds) - 1]

readMaps :: [Text.Text] -> [RangeMap Int Int]
readMaps = map readMap
  where
    readMap :: Text.Text -> RangeMap Int Int
    readMap mapText = map extractMappedRange entriesText
      where
        (_ : entriesText) = Text.lines mapText
        extractMappedRange :: Text.Text -> MappedRange Int Int
        extractMappedRange entryText = MappedRange (read $ Text.unpack target) (read $ Text.unpack source) (read $ Text.unpack entries)
          where
            [target, source, entries] = Text.split (== ' ') entryText
