import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_11/input.txt"

splitInt :: String -> [String]
splitInt x = getAllTextMatches (x =~ "[0-9]+") :: [String]

getDataFromFile :: FilePath -> IO [Integer]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  return $ map read $ splitInt $ head fileLines

-- Part 1

get_num_of_digits :: Integer -> Integer
get_num_of_digits n = toInteger (length (show n))

split_digits :: Integer -> Integer -> [Integer]
split_digits b len = [num1, b - num1 * (10 ^ half_len)]
  where
    half_len = len `div` 2
    num1 = b `div` (10 ^ half_len)

a_blink :: Integer -> Integer -> Integer
a_blink _ 0 = 1
a_blink num left
  | num == 0 = a_blink 1 (left - 1)
  | len `rem` 2 == 0 = sum $ map (\x -> a_blink x (left - 1)) (split_digits num len)
  | otherwise = a_blink (num * 2024) (left - 1)
  where
    len = get_num_of_digits num

part1 = do
  stones <- getDataFromFile f_name
  -- https://wjwh.eu/posts/2022-11-30-haskell-aoc-tricks.html
  return $ sum $ map (\x -> a_blink x 25) stones

-- Part 2
part2 = do
  stones <- getDataFromFile f_name
  return $ sum $ map (\x -> a_blink x 75) stones
