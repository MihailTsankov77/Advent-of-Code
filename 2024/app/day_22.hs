import Data.Bits (xor)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

f_name :: FilePath
f_name = "./inputs/day_22/input.txt"

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

getDataFromFile :: FilePath -> IO [Int]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents

  return $ map read fileLines

-- Part 1

mix :: Int -> Int -> Int
mix a b = xor a b

prune :: Int -> Int
prune num = num `rem` 16777216

improve_secret_number :: Int -> Int -> Int
improve_secret_number a b = prune $ mix a b

get_secret_number :: Int -> Int
get_secret_number num = num3
  where
    num1 = improve_secret_number num (num * 64)
    num2 = improve_secret_number num1 (num1 `div` 32)
    num3 = improve_secret_number num2 (num2 * 2048)

generate_secret_number :: Int -> Int -> Int
generate_secret_number 0 num = num
generate_secret_number n num = generate_secret_number (n - 1) (get_secret_number num)

part1 = do
  nums <- getDataFromFile f_name

  return $ sum $ map (generate_secret_number 2000) nums

-- part 2

type Sequences = [Int]

type NumMaxSequences = HashMap Sequences Int

generate_secret_number_init :: Int -> Int -> NumMaxSequences
generate_secret_number_init n num = generate_secret_number_part2 n num (num `rem` 10) [] HashMap.empty

set_value_in_map :: [Int] -> Int -> NumMaxSequences -> NumMaxSequences
set_value_in_map seq price map = if prev_v /= -1 then map else HashMap.insert seq price map
  where
    prev_v = fromMaybe (-1) $ HashMap.lookup seq map

generate_secret_number_part2 :: Int -> Int -> Int -> [Int] -> NumMaxSequences -> NumMaxSequences
generate_secret_number_part2 0 _ _ _ map = map
generate_secret_number_part2 n num prev_val seq map = generate_secret_number_part2 (n - 1) secret_num price new_seq new_map
  where
    secret_num = get_secret_number num
    price = secret_num `rem` 10
    diff = price - prev_val
    new_seq = (if length seq == 4 then drop 1 seq else seq) ++ [diff]
    new_map = (if length new_seq == 4 then (set_value_in_map new_seq price map) else map)

marge_into_res_by_seq :: [NumMaxSequences] -> NumMaxSequences
marge_into_res_by_seq [] = HashMap.empty
marge_into_res_by_seq (x : xs) = HashMap.unionWith (+) x (marge_into_res_by_seq xs)

part2 = do
  nums <- getDataFromFile f_name

  let maps = map (generate_secret_number_init 2000) nums
  let res_map = marge_into_res_by_seq maps

  return $ maximum $ HashMap.elems res_map