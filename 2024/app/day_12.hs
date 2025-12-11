import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

f_name :: FilePath
f_name = "./inputs/day_12/input.txt"

type Position = (Int, Int)

type Tile = (Char, Bool)

type Grid = HashMap Position Tile

getDataFromFile :: FilePath -> IO (Grid, [Position])
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents

  let grid_pos = concat [map (\(x, i) -> ((x, y), (i, False))) (zip [0 ..] line) | (y, line) <- (zip [0 ..] fileLines)]

  let grid = HashMap.fromList grid_pos

  let pos = [fst x | x <- grid_pos]

  return (grid, pos)

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

member :: Position -> [Position] -> Bool
member _ [] = False
member pos (x : xs) = if pos == x then True else member pos xs

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

-- Part 2

type FancyTile = (Position, Char)

consume_foldl :: Char -> ([FancyTile], Grid) -> Position -> ([FancyTile], Grid)
consume_foldl el (a_tile, a_grid) pos = (a_tile ++ new_tiles, grid')
  where
    (new_tiles, grid') = get_components a_grid pos el

get_components :: Grid -> Position -> Char -> ([FancyTile], Grid)
get_components grid pos el = if visited then ([], grid) else get_components' grid pos el
  where
    (_, visited) = fromMaybe (' ', True) $ HashMap.lookup pos grid

get_components' grid p@(x, y) el = foldl (consume_foldl el) (fancy, grid') next_move
  where
    options = [((x, y - 1), '^'), ((x + 1, y), '>'), ((x, y + 1), 'v'), ((x - 1, y), '<')]
    connected = filter (\(_, (e, _)) -> e == el) [(pos, fromMaybe (' ', True) $ HashMap.lookup pos grid) | (pos, _) <- options]

    connected_pos = map fst connected
    next_move = map fst $ filter (not . snd . snd) connected

    left_tile = map snd $ filter (\(pos, _) -> not $ member pos connected_pos) options

    fancy = [(p, '.')] ++ [(p, c) | c <- left_tile]

    grid' = HashMap.insert (x, y) (el, True) grid

traverse_grid :: Grid -> [Position] -> [[FancyTile]]
traverse_grid grid [] = []
traverse_grid grid (pos : rest) = if visited then traverse_grid grid rest else [fancy] ++ (traverse_grid grid' rest)
  where
    (el, visited) = fromMaybe (' ', True) $ (HashMap.lookup pos grid)
    (fancy, grid') = if visited then ([], grid) else get_components grid pos el

calculate_price_part1 :: [FancyTile] -> Int
calculate_price_part1 tiles = area * perimeter
  where
    area = length $ filter (\(pos, c) -> c == '.') tiles
    perimeter = length $ filter (\(pos, c) -> c /= '.') tiles

part1 = do
  (grid, pos) <- getDataFromFile f_name

  let tiles = traverse_grid grid pos

  return $ sum $ map calculate_price_part1 tiles

-- Part2

count_vertical :: [FancyTile] -> Int
count_vertical tiles = go tilePositions visitedMap
  where
    tilePositions = map fst tiles

    visitedMap :: HashMap Position Bool
    visitedMap = HashMap.fromList [(pos, False) | (pos, _) <- tiles]

    go :: [Position] -> HashMap Position Bool -> Int
    go [] _ = 0
    go (p : ps) grid = if visited then go ps grid else 1 + go ps (explore grid p)
      where
        visited = fromMaybe True (HashMap.lookup p grid)

    explore :: HashMap Position Bool -> Position -> HashMap Position Bool
    explore grid p = if visited then grid else explore_neighbors (HashMap.insert p True grid) p
      where
        visited = fromMaybe True (HashMap.lookup p grid)

    explore_neighbors :: HashMap Position Bool -> Position -> HashMap Position Bool
    explore_neighbors grid (x, y) = foldl explore grid next_moves
      where
        options = [(x, y - 1), (x, y + 1)]
        next_moves = [pos | pos <- options, (fromMaybe True (HashMap.lookup pos grid)) == False]

count_horizontal :: [FancyTile] -> Int
count_horizontal tiles = go tilePositions visitedMap
  where
    tilePositions = map fst tiles

    visitedMap :: HashMap Position Bool
    visitedMap = HashMap.fromList [(pos, False) | (pos, _) <- tiles]

    go :: [Position] -> HashMap Position Bool -> Int
    go [] _ = 0
    go (p : ps) grid = if visited then go ps grid else 1 + go ps (explore grid p)
      where
        visited = fromMaybe True (HashMap.lookup p grid)

    explore :: HashMap Position Bool -> Position -> HashMap Position Bool
    explore grid p = if visited then grid else explore_neighbors (HashMap.insert p True grid) p
      where
        visited = fromMaybe True (HashMap.lookup p grid)

    explore_neighbors :: HashMap Position Bool -> Position -> HashMap Position Bool
    explore_neighbors grid (x, y) = foldl explore grid next_moves
      where
        options = [(x - 1, y), (x + 1, y)]
        next_moves = [pos | pos <- options, (fromMaybe True (HashMap.lookup pos grid)) == False]

calculate_price_part2 :: [FancyTile] -> Int
calculate_price_part2 tiles = area * perimeter
  where
    area = length $ filter (\(pos, c) -> c == '.') tiles
    wall_left = filter (\(pos, c) -> c == '<') tiles
    wall_right = filter (\(pos, c) -> c == '>') tiles
    wall_up = filter (\(pos, c) -> c == '^') tiles
    wall_down = filter (\(pos, c) -> c == 'v') tiles

    perimeter = sum $ (map count_vertical [wall_left, wall_right] ++ map count_horizontal [wall_up, wall_down])

part2 = do
  (grid, pos) <- getDataFromFile f_name

  let tiles = traverse_grid grid pos

  return $ sum $ map calculate_price_part2 tiles