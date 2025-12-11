import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_19/input.txt"

splitChars :: String -> [String]
splitChars x = getAllTextMatches (x =~ "[a-z]+") :: [String]

getDataFromFile :: FilePath -> IO ([String], [String])
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  let patterns = splitChars $ head fileLines
  let levels = tail $ drop 1 fileLines
  return (patterns, levels)

-- Part 1

match_pattern :: String -> String -> Bool
match_pattern l p = take (length p) l == p

validate_levels :: [String] -> String -> Bool
validate_levels [] _ = False
validate_levels _ "" = True
validate_levels xs level = any (\mp -> (validate_levels xs (drop (length mp) level))) matched_patterns
  where
    matched_patterns = filter (match_pattern level) xs

part1 = do
  (patterns, levels) <- getDataFromFile f_name
  return $ length $ filter (validate_levels patterns) levels

-- Part 2
get_variants :: [String] -> String -> Int
get_variants [] _ = 0
get_variants _ "" = 1
get_variants xs level =
  if length matched_patterns == 0
    then 0
    else sum $ map (\mp -> (get_variants xs (drop (length mp) level))) matched_patterns
  where
    matched_patterns = filter (match_pattern level) xs

part2 = do
  (patterns, levels) <- getDataFromFile f_name
  return $ sum $ map (get_variants patterns) levels
