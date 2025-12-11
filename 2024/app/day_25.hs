import Data.List (transpose)

f_name :: FilePath
f_name = "./inputs/day_25/input.txt"

split_by_cages :: [String] -> [[String]]
split_by_cages [] = []
split_by_cages (x : xs) = (x : takeWhile (not . null) xs) : split_by_cages (drop 1 $ dropWhile (not . null) xs)

map_cage :: [String] -> [Int]
map_cage = count_cage . transpose

count_cage :: [String] -> [Int]
count_cage [] = []
count_cage (x : xs) = length (filter (\c -> c == '#') x) : count_cage xs

getDataFromFile :: FilePath -> IO ([[Int]], [[Int]])
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  let cages = split_by_cages fileLines

  let keys = filter (\x -> head (head x) == '.') cages
  let locks = filter (\x -> head (head x) == '#') cages

  let mapped_keys = map map_cage keys
  let mapped_locks = map map_cage locks

  return (mapped_keys, mapped_locks)

-- Part 1

match :: [Int] -> [Int] -> Bool
match [] [] = True
match (x : xs) (y : ys) = x + y <= 7 && match xs ys

count_matches :: [[Int]] -> [Int] -> Int
count_matches keys lock = length $ filter (match lock) keys

part1 = do
  (keys, locks) <- getDataFromFile f_name

  return $ sum $ map (count_matches keys) locks
