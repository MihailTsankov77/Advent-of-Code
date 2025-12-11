import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_07/input.txt"

splitInt :: String -> [String]
splitInt x = getAllTextMatches (x =~ "[0-9]+") :: [String]

getDataFromFile :: FilePath -> IO [(Int, [Int])]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  return [(read (head nums), map read (tail nums)) | nums <- map (splitInt) fileLines]

-- Part 1

is_valid_row :: (Int, [Int]) -> Bool
is_valid_row (sum_val, (x : nums)) = validate sum_val x nums
  where
    validate s r [] = s == r
    validate sum_val x (n : nums) = validate sum_val (x + n) nums || validate sum_val (x * n) nums

part1 = do
  rows <- getDataFromFile f_name
  let valid_rows = filter is_valid_row rows
  return $ sum $ map fst valid_rows

-- Part 2

concatInts :: Int -> Int -> Int
concatInts x y = x * (10 ^ (floor (logBase 10 (fromIntegral y)) + 1)) + y

is_valid_row' :: (Int, [Int]) -> Bool
is_valid_row' (sum_val, (x : nums)) = validate sum_val x nums
  where
    validate s r [] = s == r
    validate sum_val x (n : nums) = validate sum_val (x + n) nums || validate sum_val (x * n) nums || validate sum_val (concatInts x n) nums

part2 = do
  rows <- getDataFromFile f_name
  let valid_rows = filter is_valid_row' rows
  return $ sum $ map fst valid_rows
