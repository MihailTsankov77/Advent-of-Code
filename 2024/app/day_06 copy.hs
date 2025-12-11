import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Debug.Trace
import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_06/input_test.txt"

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

lookupWithDefault :: (Eq k, Hashable k) => a -> k -> HashMap k a -> a
lookupWithDefault def key hashmap = fromMaybe def (HashMap.lookup key hashmap)

type Position = (Int, Int)

type Grid = HashMap Position Int

type Dimension = (Int, Int)

getDataFromFile :: FilePath -> IO (Grid, Position, Dimension)
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents

  let grid_pos = concat [map (\(x, i) -> ((x, y), i)) (zip [0 ..] line) | (y, line) <- (zip [0 ..] fileLines)]

  let grid = HashMap.fromList $ map (\(p, el) -> (p, if el == '#' then 1 else 0)) grid_pos

  let init_pos = fst $ head $ filter (\(_, el) -> el == '^') grid_pos

  let width = length $ head fileLines
  let height = length fileLines

  return (grid, init_pos, (width, height))

-- Part 1

get_movement :: Int -> Position -> Position
get_movement 0 (x, y) = (x, y - 1)
get_movement 1 (x, y) = (x + 1, y)
get_movement 2 (x, y) = (x, y + 1)
get_movement 3 (x, y) = (x - 1, y)

is_outside :: Dimension -> Position -> Bool
is_outside (width, height) (x, y) = x < 0 || y < 0 || x >= width || y >= height

rotate :: Int -> Int
rotate x = (x + 1) `mod` 4

type Visited = HashMap Position Bool

consume :: Grid -> Position -> Dimension -> Int -> Visited -> Visited
consume grid pos dim rotation visited =
  if is_outside dim pos
    then visited
    else
      ( if next_value == 1
          then consume grid pos dim (rotate rotation) visited'
          else consume grid next_pos dim rotation visited'
      )
  where
    visited' = HashMap.insert pos True visited

    next_pos = get_movement rotation pos
    next_value = lookupWithDefault 0 next_pos grid

part1 = do
  (grid, pos, dim) <- getDataFromFile f_name

  let visited = consume grid pos dim 0 HashMap.empty

  return $ length $ filter (\(_, el) -> el) $ HashMap.toList visited

-- Part 2

check_is_cycle :: Grid -> (Position, Int) -> Position -> Dimension -> Int -> Bool
check_is_cycle grid init pos dim rotation =
  if init == (pos, rotation)
    then True
    else
      ( if is_outside dim pos
          then False
          else
            ( if next_value == 1
                then check_is_cycle grid init pos dim (rotate rotation)
                else check_is_cycle grid init next_pos dim rotation
            )
      )
  where
    next_pos = get_movement rotation pos
    next_value = lookupWithDefault 0 next_pos grid

consume_part2 :: Grid -> Position -> Dimension -> Int -> Int -> HashMap Position [Int] -> Int
consume_part2 grid pos dim rotation count visited =
  if is_outside dim pos
    then count
    else
      ( if next_value == 1
          then consume_part2 grid pos dim (rotate rotation) count' visited'
          else consume_part2 grid next_pos dim rotation count' visited'
      )
  where
    until_now = lookupWithDefault [] pos visited
    visited' = HashMap.insert pos (until_now ++ [rotation]) visited

    next_pos = get_movement rotation pos
    next_value = lookupWithDefault 0 next_pos grid

    next_rotations = rotate rotation

    count' =
      if next_rotations `elem` until_now
        then count + 1
        else count

-- was_visited = lookupWithDefault False pos visited

-- count' =
--   if was_visited && (check_is_cycle (HashMap.insert (get_movement rotation next_pos) 1 grid) (next_pos, rotation) next_pos dim (rotate rotation))
--     then count + 1
--     else count

part2 = do
  (grid, pos, dim) <- getDataFromFile f_name

  let count = consume_part2 grid pos dim 0 0 HashMap.empty

  return count