import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

f_name :: FilePath
f_name = "./inputs/day_08/input.txt"

getDataFromFile :: FilePath -> IO (HashMap Char [(Int, Int)], Int, Int)
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents

  let width = length $ head fileLines
  let height = length fileLines

  let sat = concat $ filter (not . null) [map (\(x, i) -> (i, (x, y))) (filter (\(x, i) -> i /= '.') (zip [0 ..] line)) | (y, line) <- (zip [0 ..] fileLines)]

  let hashMap = foldl (\acc (k, v) -> HashMap.insertWith (++) k [v] acc) HashMap.empty sat
  return (hashMap, width, height)

-- Part 1

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

subArr :: (Eq a) => [a] -> [[a]]
subArr [] = []
subArr xs = unique [[x, y] | x <- xs, y <- xs, x /= y]

get_anti_node :: [(Int, Int)] -> [(Int, Int)]
get_anti_node ((x1, y1) : (x2, y2) : _) = [(x1', y1'), (x2', y2')]
  where
    dx = abs (x2 - x1)
    dy = abs (y2 - y1)

    x1' = if x1 < x2 then x1 - dx else x1 + dx
    y1' = if y1 < y2 then y1 - dy else y1 + dy

    x2' = if x1 < x2 then x2 + dx else x2 - dx
    y2' = if y1 < y2 then y2 + dy else y2 - dy
get_anti_node _ = []

find_antinode :: [(Int, Int)] -> [(Int, Int)]
find_antinode [] = []
find_antinode xs = concatMap get_anti_node (subArr xs)

is_in_bounds :: (Int, Int) -> Int -> Int -> Bool
is_in_bounds (x, y) w h = x >= 0 && x < w && y >= 0 && y < h

part1 = do
  (hashmap, w, h) <- getDataFromFile f_name

  -- GPT-4o: Traverse hashmap elements
  let rows = HashMap.foldrWithKey (\_ v acc -> (find_antinode v) ++ acc) [] hashmap
  let in_bounds = filter (\(x, y) -> is_in_bounds (x, y) w h) rows

  return $ length $ unique in_bounds

-- Part 2

get_anti_node_in_bounds :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
get_anti_node_in_bounds w h (a@(x1, y1) : b@(x2, y2) : _) = a1 ++ a2
  where
    dx = abs (x2 - x1)
    dy = abs (y2 - y1)

    x1_symbol = if x1 < x2 then -1 else 1
    y1_symbol = if y1 < y2 then -1 else 1
    x2_symbol = if x1 < x2 then 1 else -1
    y2_symbol = if y1 < y2 then 1 else -1

    a1 = takeWhile (\x -> is_in_bounds x w h) $ iterate (\(x, y) -> (x + x1_symbol * dx, y + y1_symbol * dy)) a
    a2 = takeWhile (\x -> is_in_bounds x w h) $ iterate (\(x, y) -> (x + x2_symbol * dx, y + y2_symbol * dy)) b
get_anti_node_in_bounds _ _ _ = []

find_antinode_in_bounds :: Int -> Int -> [(Int, Int)] -> [(Int, Int)]
find_antinode_in_bounds w h xs = concatMap (get_anti_node_in_bounds w h) (subArr xs)

part2 = do
  (hashmap, w, h) <- getDataFromFile f_name

  -- GPT-4o: Traverse hashmap elements
  let rows = HashMap.foldrWithKey (\_ v acc -> (find_antinode_in_bounds w h v) ++ acc) [] hashmap

  return $ length $ unique rows
