import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (intercalate, maximumBy, sort, sortBy)
import Data.Set (Set)
import qualified Data.Set as Set
import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_23/input.txt"

splitChars :: String -> [String]
splitChars x = getAllTextMatches (x =~ "[a-z]+") :: [String]

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

startsWithT :: String -> Bool
startsWithT x = take 1 x == "t"

getDataFromFile :: FilePath -> IO ([String], HashMap String [String], [String])
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  let links = map splitChars fileLines
  let keys = unique $ concat links
  let keysT = filter startsWithT $ keys
  let hashMap =
        foldl
          (\acc [k, v] -> HashMap.insertWith (++) k [v] acc)
          HashMap.empty
          (links ++ (map reverse links))
  return (keysT, hashMap, keys)

-- Part 1

subArr :: (Eq a, Ord a) => [a] -> [[a]]
subArr [] = []
subArr xs = unique [sort [x, y] | x <- xs, y <- xs, x /= y]

find_s :: String -> HashMap String [String] -> [[String]]
find_s key hashMap = map (\a -> sort ([key] ++ a)) (filter (\[x, y] -> x `elem` (hashMap HashMap.! y)) subs)
  where
    subs :: [[String]]
    subs = subArr (hashMap HashMap.! key)

part1 = do
  (keysT, hashMap, _) <- getDataFromFile f_name

  return $ length $ unique $ concatMap (\k -> find_s k hashMap) keysT

-- Part 2

combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations 1 xs = map (: []) xs
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

sorted_combinations :: (Eq a) => Int -> [a] -> [[a]]
sorted_combinations n xs = sortBy (\a b -> compare (length b) (length a)) (concatMap (\i -> (combinations i xs)) [2 .. n])

verify :: [String] -> HashMap String [String] -> Bool
verify [] _ = True
verify xs hashMap = all (\[x, y] -> x `elem` (hashMap HashMap.! y)) comps
  where
    comps = subArr xs

-- filter (\x -> verify x hashMap) (sorted_combinations maxNeighbors keys)

get_max_var :: HashMap String [String] -> String -> [String]
get_max_var hashMap key = [key] ++ max_e
  where
    keys = hashMap HashMap.! key
    maxNeighbors = min (length keys) (snd $ maximum $ map (\k -> (k, length (hashMap HashMap.! k))) keys)
    fileted = filter (\x -> verify x hashMap) (sorted_combinations maxNeighbors keys)
    max_e = maximumBy (\a b -> compare (length a) (length b)) fileted

part2 = do
  (_, hashMap, keys) <- getDataFromFile f_name

  return $ intercalate "," $ sort $ maximumBy (\a b -> compare (length a) (length b)) $ map (get_max_var hashMap) keys