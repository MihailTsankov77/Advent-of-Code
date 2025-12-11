import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_11/input.txt"

splitInt :: String -> [String]
splitInt x = getAllTextMatches (x =~ "[0-9]+") :: [String]

getDataFromFile :: FilePath -> IO [Int]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  return $ map read $ splitInt $ head fileLines

-- Part 1

get_num_of_digits :: Int -> Int
get_num_of_digits n = length (show n)

split_digits :: Int -> Int -> [Int]
split_digits b len = [num1, b - num1 * (10 ^ half_len)]
  where
    half_len = len `div` 2
    num1 = b `div` (10 ^ half_len)

a_blink :: [Int] -> [Int]
a_blink [] = []
a_blink (x : xs)
  | x == 0 = 1 : a_blink xs
  | len `rem` 2 == 0 = (split_digits x len) ++ a_blink xs
  | otherwise = (x * 2024) : a_blink xs
  where
    len = get_num_of_digits x

part1 = do
  stones <- getDataFromFile f_name
  -- https://wjwh.eu/posts/2022-11-30-haskell-aoc-tricks.html
  return $ length $ last $ take 26 $ iterate a_blink stones

-- Part 2
part2 = do
  stones <- getDataFromFile f_name
  return $ length $ last $ take 76 $ iterate a_blink stones
