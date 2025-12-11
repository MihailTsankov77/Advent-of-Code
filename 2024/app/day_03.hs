-- Source: https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()

day_02_input :: FilePath
day_02_input = "./inputs/day_03/input.txt"

getDataFromFile :: FilePath -> IO String
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  return $ concat fileLines

-- Part 1

m_regex:: String
m_regex = "mul\\([0-9]+,[0-9]+\\)"

part1 = do
  filedata <- getDataFromFile day_02_input
  let matches = getAllTextMatches (filedata =~ m_regex) :: [String]
  let numbers = map (map (read :: String -> Integer)) $ map (\x -> getAllTextMatches(x =~ "[0-9]+") :: [String]) matches
  return $ sum $ map (\[x,y] -> x * y) numbers


-- Part 2

m_regex'instr:: String
m_regex'instr = "do\\(\\)|don't\\(\\)|mul\\([0-9]+,[0-9]+\\)"

clean_up :: [String] -> [String]
clean_up = clean True
  where
    clean:: Bool -> [String] -> [String]
    clean _ [] = []
    clean en (x:xs)
      | x == "do()" = clean True xs
      | x == "don't()" = clean False xs
      | en = x : clean en xs
      | otherwise = clean en xs

part2 = do
  filedata <- getDataFromFile day_02_input
  let matches = getAllTextMatches (filedata =~ m_regex'instr) :: [String]
  let cleaned_matches = clean_up matches
  let numbers = map (map (read :: String -> Integer)) $ map (\x -> getAllTextMatches(x =~ "[0-9]+") :: [String]) cleaned_matches
  return $ sum $ map (\[x,y] -> x * y) numbers
