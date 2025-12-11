day_02_input :: FilePath
day_02_input = "./inputs/day_02/input.txt"

getDataFromFile :: FilePath -> IO [[Integer]]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  let wordsList = map words fileLines
  return $ map (map read) wordsList


-- Part 1
validate_level :: [Integer] -> Bool
validate_level = is_valid 0 
  where
    is_valid:: Integer -> [Integer] -> Bool
    is_valid _ [] = True
    is_valid 0 (x:xs)
      | length xs == 0 = True
      | diff >= 1  && diff <=3 = is_valid 3 xs
      | diff <= -1  && diff >= -3 = is_valid 1 xs
      | otherwise = False
      where
        diff = x - head xs
    is_valid mode (x:xs)
      | length xs == 0 = True
      | diff >= 1  && diff <=3 = is_valid mode xs
      | otherwise = False
      where
        diff = (x - head xs) * (mode - 2)

part1 = do
  filedata <- getDataFromFile day_02_input
  return $ length $ filter validate_level filedata

-- Part 2

get_vector_combinations :: [Integer] -> [[Integer]]
get_vector_combinations [] = []
get_vector_combinations xs = map (rem) [-1..(length xs - 1)]
  where 
    rem i = take i xs ++ drop (i+1) xs

validate_level_with_bad :: [Integer] -> Bool
validate_level_with_bad xs = any validate_level (get_vector_combinations xs)


part2 = do
  filedata <- getDataFromFile day_02_input
  return $ length $ filter validate_level_with_bad filedata