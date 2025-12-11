import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List (sort, sortBy)

f_name :: FilePath
f_name = "./inputs/day_24/input.txt"

type Values = HashMap String Int

type Gate = (String, [String], String)

type Gates = [Gate]

type ZValues = [String]

remove_last :: [a] -> [a]
remove_last [] = []
remove_last [x] = []
remove_last (x : xs) = x : remove_last xs

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

lookup_with_default :: (Eq k, Hashable k) => v -> k -> HashMap k v -> v
lookup_with_default d k m = fromMaybe d (HashMap.lookup k m)

getDataFromFile :: FilePath -> IO (Values, Gates, ZValues, [String])
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
      (init, rest) = break null fileLines
      gates' = drop 1 rest

  let init_values = HashMap.fromList $ map (\[x, y] -> (remove_last x, read y :: Int)) $ map words init

  let gates = map (\[v1, o, v2, _, r] -> (o, [v1, v2], r)) $ map words gates'

  let inInit v1 v2 = (HashMap.member v1 init_values, HashMap.member v2 init_values)

  let sorted_gates = sortBy (\(_, [v1, v2], _) (_, [v1', v2'], _) -> compare (inInit v1 v2) (inInit v1' v2')) gates

  let keys = map (\[_, _, _, _, r] -> r) $ map words gates'
  let zValues = sort $ filter (\r -> head r == 'z') keys

  return (init_values, gates, zValues, keys)

-- Part 1

solve_and :: Values -> Gate -> (Values, Bool)
solve_and values (o, [v1, v2], r) =
  if 0 `elem` v_values
    then
      (HashMap.insert r 0 values, True)
    else
      if -1 `elem` v_values
        then
          (values, False)
        else
          (HashMap.insert r 1 values, True)
  where
    v_values = map (\v -> lookup_with_default (-1) v values) [v1, v2]

solve_xor :: Values -> Gate -> (Values, Bool)
solve_xor values (o, [v1, v2], r) =
  if -1 `elem` v_values
    then
      (values, False)
    else
      (HashMap.insert r (if (v_values !! 0) /= (v_values !! 1) then 1 else 0) values, True)
  where
    v_values = map (\v -> lookup_with_default (-1) v values) [v1, v2]

solve_or :: Values -> Gate -> (Values, Bool)
solve_or values (o, [v1, v2], r) =
  if 1 `elem` v_values
    then
      (HashMap.insert r 1 values, True)
    else
      if -1 `elem` v_values
        then
          (values, False)
        else
          (HashMap.insert r 0 values, True)
  where
    v_values = map (\v -> lookup_with_default (-1) v values) [v1, v2]

solve_gate :: Values -> Gate -> (Values, Bool)
solve_gate values g@(o, _, _)
  | o == "AND" = solve_and values g
  | o == "OR" = solve_or values g
  | o == "XOR" = solve_xor values g
  | otherwise = error ("Unknown gate" ++ o)

solve_gates :: Values -> Gates -> Values
solve_gates values [] = values
solve_gates values (g : gs) =
  if solved
    then
      solve_gates new_values gs
    else
      solve_gates values (gs ++ [g])
  where
    (new_values, solved) = solve_gate values g

calculate_res :: Values -> ZValues -> Int -> Int
calculate_res values [] pw = 0
calculate_res values (k : keys) pw = v * power + calculate_res values keys (pw + 1)
  where
    v = lookup_with_default (-1) k values
    power = 2 ^ pw

part1 = do
  (values, gates, zValues, _) <- getDataFromFile f_name

  let solved_values = solve_gates values gates

  return $ calculate_res solved_values zValues 0

-- Part 2

combinations :: (Eq a) => Int -> [a] -> [[a]]
combinations 0 _ = [[]]
combinations _ [] = []
combinations 1 xs = map (: []) xs
combinations n (x : xs) = map (x :) (combinations (n - 1) xs) ++ combinations n xs

-- get combinations of 8 elements
-- group them by 2 -> make all combinations
-- check if with swap will be valid (you need to calculate x & y)
-- to optimize we can find the safe nodes and check only the others