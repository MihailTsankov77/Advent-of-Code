import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Text.Regex.TDFA

split_int :: String -> Int
split_int x = read (x =~ "[1-9][0-9]*" :: String) :: Int

f_name :: FilePath
f_name = "./inputs/day_21/input.txt"

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

getDataFromFile :: FilePath -> IO [String]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents

  return fileLines

-- Part 1

num_pad_locations :: HashMap Char (Int, Int)
num_pad_locations =
  HashMap.fromList
    [ ('7', (0, 0)),
      ('8', (1, 0)),
      ('9', (2, 0)),
      ('4', (0, 1)),
      ('5', (1, 1)),
      ('6', (2, 1)),
      ('1', (0, 2)),
      ('2', (1, 2)),
      ('3', (2, 2)),
      ('0', (1, 3)),
      ('A', (2, 3))
    ]

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

get_paths_to :: HashMap Char (Int, Int) -> (Int, Int) -> Char -> Char -> [[Char]]
get_paths_to pos exclude from to = map (\x -> (map snd x) ++ ['A']) $ unique $ filter (\x -> not $ (exclude `elem`) $ map fst x) [variant_1, variant_2]
  where
    (from_x, from_y) = fromMaybe (-1, -1) $ HashMap.lookup from pos
    (to_x, to_y) = fromMaybe (-1, -1) $ HashMap.lookup to pos

    x_diff = to_x - from_x
    y_diff = to_y - from_y

    horizontal_movement = if x_diff > 0 then [from_x + 1 .. to_x] else [from_x - 1, from_x - 2 .. to_x]
    vertical_movement = if y_diff > 0 then [from_y + 1 .. to_y] else [from_y - 1, from_y - 2 .. to_y]

    hor_move = if x_diff > 0 then '>' else '<'
    ver_move = if y_diff > 0 then 'v' else '^'

    variant_1 = (map (\c -> (c, hor_move)) $ zip horizontal_movement (repeat from_y)) ++ (map (\c -> (c, ver_move)) $ zip (repeat to_x) vertical_movement)
    variant_2 = (map (\c -> (c, ver_move)) $ zip (repeat from_x) vertical_movement) ++ (map (\c -> (c, hor_move)) $ zip horizontal_movement (repeat to_y))

control_panel_locations :: HashMap Char (Int, Int)
control_panel_locations =
  HashMap.fromList
    [ ('^', (1, 0)),
      ('v', (1, 1)),
      ('<', (0, 1)),
      ('>', (2, 1)),
      ('A', (2, 0))
    ]

get_num_pad_paths_to :: Char -> Char -> [[[Char]]]
get_num_pad_paths_to f t = [get_paths_to num_pad_locations (0, 3) f t]

get_control_panel_paths_to :: Char -> Char -> [[[Char]]]
get_control_panel_paths_to f t = [get_paths_to control_panel_locations (0, 0) f t]

get_shortest_path :: (Char -> Char -> [[[Char]]]) -> String -> [String]
get_shortest_path fn str = all_combinations
  where
    paths = snd $ foldl (\(from, acc) to -> (to, acc ++ fn from to)) ('A', []) str

    all_combinations = map (foldl (++) "") $ sequence paths

    shortest_path = minimum $ map length all_combinations

    only_shortest = filter (\x -> length x == shortest_path) all_combinations

get_all_shortest_paths :: (Char -> Char -> [[[Char]]]) -> [String] -> [String]
get_all_shortest_paths _ [] = []
get_all_shortest_paths fn xs = only_shortest
  where
    all_combinations = concatMap (get_shortest_path fn) xs

    shortest_path = minimum $ map length all_combinations

    only_shortest = filter (\x -> length x == shortest_path) all_combinations

get_all_sp_num_pad :: String -> [String]
get_all_sp_num_pad x = get_all_shortest_paths get_num_pad_paths_to [x]

get_all_sp_control_panel :: [String] -> [String]
get_all_sp_control_panel = get_all_shortest_paths get_control_panel_paths_to

get_final_path_part1 :: String -> Int
get_final_path_part1 = length . head . get_all_sp_control_panel . get_all_sp_control_panel . get_all_sp_num_pad

get_score :: String -> Int
get_score str = (split_int str) * (get_final_path_part1 str)

part1 = do
  lines <- getDataFromFile f_name

  return $ sum $ map get_score lines

-- Part 2

type Mem = HashMap [String] [String]

get_all_shortest_paths_memoized :: Mem -> (Char -> Char -> [[String]]) -> [String] -> (Mem, [String])
get_all_shortest_paths_memoized mem fn xs = if length memo_res /= 0 then (mem, memo_res) else get_res xs
  where
    memo_res = fromMaybe [] $ HashMap.lookup xs mem

    get_res xs = (HashMap.insert xs r mem, r)
      where
        r = get_all_shortest_paths fn xs

get_all_sp_control_panel_part2 :: Mem -> [String] -> (Mem, [String])
get_all_sp_control_panel_part2 mem = get_all_shortest_paths_memoized mem get_control_panel_paths_to

repeat_robots :: Int -> Mem -> [String] -> (Mem, [String])
repeat_robots 0 mem str = (mem, str)
repeat_robots n mem str = repeat_robots (n - 1) mem' str'
  where
    (mem', str') = (get_all_sp_control_panel_part2 mem str)

get_final_path_part2 :: String -> Mem -> (Mem, Int)
get_final_path_part2 str mem = (mem', length $ head paths)
  where
    (mem', paths) = (repeat_robots 26 mem) $ get_all_sp_num_pad str

get_score_part2 :: String -> Mem -> (Mem, Int)
get_score_part2 str mem = (mem', (split_int str) * len)
  where
    (mem', len) = (get_final_path_part2 str mem)

result_foldl_cb :: (Mem, Int) -> String -> (Mem, Int)
result_foldl_cb (mem, res) x = (mem', res + calc_res)
  where
    (mem', calc_res) = (get_score_part2 x mem)

part2 = do
  lines <- getDataFromFile f_name

  let res = snd $ foldl result_foldl_cb (HashMap.empty, 0) lines

  return res
