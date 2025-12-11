import qualified Data.HashMap.Strict as HashMap
import Text.Regex.TDFA
import Data.Hashable (Hashable)
import qualified Data.Maybe


f_name :: FilePath
f_name = "./inputs/day_05/input.txt"

splitInt:: String -> [String]
splitInt x = getAllTextMatches (x  =~ "[0-9]+") :: [String]

lookupWithDefault :: (Eq k, Hashable k) => k -> a -> HashMap.HashMap k a -> a
lookupWithDefault key def hashmap =
  Data.Maybe.fromMaybe def (HashMap.lookup key hashmap)

getDataFromFile :: FilePath -> IO ((HashMap.HashMap Int [Int]), [[Int]])
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
    -- GPT-4o: split the liens to two: the one before the new line and the one after it
      (ruleLines, rest) = break null fileLines
      updateLines = drop 1 rest

  let pairs = [(read b, read a) | [a, b] <- map (splitInt) ruleLines]

  -- GPT-4o: create me a hashmap from pairs where the second element is the key and the first element is the value and the values must be in array
  let rules = foldr (\(k, v) acc -> HashMap.insertWith (++) k [v] acc) HashMap.empty pairs

  let updates = map (map read . splitInt) updateLines

  return (rules, updates)

-- Part 1

is_valid_update:: HashMap.HashMap Int [Int] -> [Int] -> Bool
is_valid_update _ [] = True
is_valid_update rules (x:xs) = not (any (`elem` (lookupWithDefault x [] rules)) xs) && is_valid_update rules xs

get_middle_element:: [Int] -> Int
-- GPT-4o: easy way to get element on index in array
get_middle_element x = x !! (length x `div` 2)

part1 = do
  (rules, updates) <- getDataFromFile f_name
  let valid_rules = filter (is_valid_update rules) updates
  return $ sum $ map get_middle_element valid_rules


-- Part 2

make_update_valid:: HashMap.HashMap Int [Int] -> [Int] -> [Int]
make_update_valid rules [] = []
make_update_valid rules (x:xs)
  | null out_of_order = x : make_update_valid rules xs
  | otherwise = make_update_valid rules (out_of_order ++ [x] ++ in_order)
  where
    out_of_order = filter (`elem` (lookupWithDefault x [] rules)) xs
    in_order = filter (not . (`elem` (lookupWithDefault x [] rules))) xs

part2 = do
  (rules, updates) <- getDataFromFile f_name
  let not_valid_rules = filter (not . is_valid_update rules) updates

  let valid_rules = map (make_update_valid rules) not_valid_rules

  return $ sum $ map get_middle_element valid_rules

