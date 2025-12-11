import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

f_name :: FilePath
f_name = "./inputs/day_10/input.txt"

type Position = (Int, Int)

type Grid = HashMap Position [(Position, Bool)]

get_neighbors :: Position -> Int -> [((Int, Int), Int)] -> [(Position, Bool)]
get_neighbors (x, y) el grid_pos = map (\(pos, el) -> (pos, el == 9)) $ filter (\x -> x `elem` grid_pos) options
  where
    options = [(((x + 1), y), el + 1), (((x - 1), y), el + 1), ((x, (y + 1)), el + 1), ((x, (y - 1)), el + 1)]

getDataFromFile :: FilePath -> IO (Grid, [(Position, Bool)])
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents

  let grid_pos = concat [map (\(x, i) -> ((x, y), read [i] :: Int)) (zip [0 ..] line) | (y, line) <- (zip [0 ..] fileLines)]

  let initial_pos = map (\(pos, _) -> (pos, False)) $ filter (\(pos, el) -> el == 0) grid_pos

  let grid = HashMap.fromList $ map (\(pos, el) -> (pos, get_neighbors pos el grid_pos)) grid_pos

  return (grid, initial_pos)

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

-- Part 1

dfs :: Grid -> (Position, Bool) -> [Position]
dfs grid (pos, end) = if end then [pos] else (concatMap (dfs grid) neighbors)
  where
    neighbors = fromMaybe [] $ HashMap.lookup pos grid

part1 = do
  (grid, initial_pos) <- getDataFromFile f_name

  return $ sum $ map (length . unique . dfs grid) initial_pos

-- Part 2
part2 = do
  (grid, initial_pos) <- getDataFromFile f_name

  return $ sum $ map (length . dfs grid) initial_pos