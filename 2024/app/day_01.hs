import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List (sort)
import qualified Data.Maybe

-- Source: https://wjwh.eu/posts/2022-11-30-haskell-aoc-tricks.html

getPairs :: FilePath -> IO [(Integer, Integer)]
getPairs fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  let wordLists = map words fileLines
  let pairLists = map (\[x, y] -> (read x, read y)) wordLists :: [(Integer, Integer)]
  return pairLists

part1 = do
  pairs <- getPairs "./inputs/day_01/input.txt"
  let firstList = sort (map fst pairs)
  let secondList = sort (map snd pairs)
  return (sum (zipWith (\x y -> abs (x - y)) secondList firstList))

-- GPT-4o: Default to value if key not found counts HashMap.! x:
-- lookupWithDefault :: (Eq k, Hashable k) => v -> k -> HashMap.HashMap k v -> v
-- lookupWithDefault def key hashmap =
--     case HashMap.lookup key hashmap of
--         Just value -> value
--         Nothing    -> def
lookupWithDefault :: (Eq k, Hashable k) => k -> HashMap.HashMap k Integer -> Integer
lookupWithDefault key hashmap =
  Data.Maybe.fromMaybe 0 (HashMap.lookup key hashmap)

-- Help: https://mmhaskell.com/data-structures/hash-map
part2 = do
  pairs <- getPairs "./inputs/day_01/input.txt"
  let firstList = sort (map fst pairs)
  let counts = HashMap.fromListWith (+) [(x, 1) | x <- map snd pairs]
  return (sum (map (\x -> lookupWithDefault x counts * x) firstList))