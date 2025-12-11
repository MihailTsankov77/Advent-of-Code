import Data.Bits (xor)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.List (intercalate)
import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_17/input.txt"

splitInt :: String -> [String]
splitInt x = getAllTextMatches (x =~ "[0-9]+") :: [String]

type Values = HashMap Char Int

type Commands = [Int]

getDataFromFile :: FilePath -> IO (Values, Commands)
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
      -- GPT-4o: split the liens to two: the one before the new line and the one after it
      (stores, rest) = break null fileLines
      cm = drop 1 rest

  let values = HashMap.fromList $ map (\[_, k, v] -> (head k, read v)) $ map words stores

  let commands = map read $ splitInt $ head cm
  return (values, commands)

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

get_hashmap_value :: Char -> Values -> Int
get_hashmap_value key values = fromMaybe 0 (HashMap.lookup key values)

-- Part 1

get_operator_value :: Int -> Values -> Int
get_operator_value x values
  | x == 4 = get_hashmap_value 'A' values
  | x == 5 = get_hashmap_value 'B' values
  | x == 6 = get_hashmap_value 'C' values
  | x >= 0 && x <= 3 = x
  | otherwise = error ("Invalid operator" ++ show x)

op_1 :: Int -> Values -> Values
op_1 x values = (HashMap.insert 'B' (b_val `xor` x) values)
  where
    b_val = get_hashmap_value 'B' values

div_op :: Int -> Char -> Values -> Values
div_op x key values = (HashMap.insert key (a_val `div` divider) values)
  where
    a_val = get_hashmap_value 'A' values
    divider = 2 ^ (get_operator_value x values)

op_2 :: Int -> Values -> Values
op_2 x values = (HashMap.insert 'B' (x_val `mod` 8) values)
  where
    x_val = get_operator_value x values

op_4 :: Values -> Values
op_4 values = (HashMap.insert 'B' (b_val `xor` c_val) values)
  where
    b_val = get_hashmap_value 'B' values
    c_val = get_hashmap_value 'C' values

op_3 :: Int -> Values -> Commands -> Commands -> Commands
op_3 x values commands cur_commands = if a_val == 0 then cur_commands else drop x commands
  where
    a_val = get_hashmap_value 'A' values
    x_val = get_operator_value x values

consume :: Values -> Commands -> Commands -> [Int]
consume _ _ [] = []
consume values init_commands (op : v : commands)
  | op == 0 = consume (div_op v 'A' values) init_commands commands
  | op == 1 = consume (op_1 v values) init_commands commands
  | op == 2 = consume (op_2 v values) init_commands commands
  | op == 3 = consume values init_commands (op_3 v values init_commands commands)
  | op == 4 = consume (op_4 values) init_commands commands
  | op == 5 = [(get_operator_value v values) `mod` 8] ++ (consume values init_commands commands)
  | op == 6 = consume (div_op v 'B' values) init_commands commands
  | op == 7 = consume (div_op v 'C' values) init_commands commands
  | otherwise = error ("Invalid command" ++ show op)

part1 = do
  (values, commands) <- getDataFromFile f_name
  let result = consume values commands commands
  return $ intercalate "," $ map show result