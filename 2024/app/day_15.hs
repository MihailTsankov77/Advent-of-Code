import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap

f_name :: FilePath
f_name = "./inputs/day_15/input.txt"

type Grid = HashMap (Int, Int) Char

type Movement = (Int, Int)

type Movements = [Movement]

type Position = (Int, Int)

map_movement :: Char -> Movement
map_movement '>' = (1, 0)
map_movement '<' = (-1, 0)
map_movement '^' = (0, -1)
map_movement 'v' = (0, 1)

getDataFromFile :: FilePath -> IO (Grid, Movements, Position)
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
      (grid, rest) = break null fileLines
      movements_st = drop 1 rest

  let grid_pos = concat [map (\(x, i) -> ((x, y), i)) (zip [0 ..] line) | (y, line) <- (zip [0 ..] grid)]

  let initial_pos = fst $ head $ filter (\(pos, el) -> el == '@') grid_pos

  let hashMap = HashMap.fromList grid_pos

  let movements = map map_movement (concat movements_st)

  return (hashMap, movements, initial_pos)

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

-- Part 1

move :: Position -> Movement -> Position
move (x, y) (dx, dy) = (x + dx, y + dy)

consume_movement :: Grid -> Position -> Movement -> (Grid, Position)
consume_movement grid pos movement = (end_grid, end_pos)
  where
    cur_element = (fromMaybe '#' (HashMap.lookup pos grid))
    new_pos = move pos movement

    next_element = (fromMaybe '#' (HashMap.lookup new_pos grid))

    consume_next_element :: Grid -> Char -> Grid
    consume_next_element grid '.' = HashMap.insert new_pos cur_element (HashMap.insert pos '.' grid)
    consume_next_element grid '#' = grid
    consume_next_element grid 'O' = fst $ consume_movement grid new_pos movement
    consume_next_element _ e = error ("Invalid element: " ++ [e])

    new_grid = consume_next_element grid next_element

    element_new_pos = HashMap.lookup new_pos new_grid

    end_grid =
      if element_new_pos == Just '.'
        then HashMap.insert new_pos cur_element (HashMap.insert pos '.' new_grid)
        else new_grid

    end_pos =
      if HashMap.lookup new_pos end_grid == Just cur_element
        then new_pos
        else pos

calculate_result :: Grid -> Int
calculate_result grid = sum $ map (\((x, y), _) -> x + (100 * y)) $ filter (\(pos, el) -> el == 'O') (HashMap.toList grid)

part1 = do
  (grid, movements, initial_pos) <- getDataFromFile f_name

  let (new_grid, end_pos) = foldl (\(gr, pos) movement -> consume_movement gr pos movement) (grid, initial_pos) movements

  return $ calculate_result new_grid

-- Part 2

make_sq_bigger :: ((Int, Int), Char) -> [((Int, Int), Char)]
make_sq_bigger ((x, y), el)
  | el == '#' || el == '.' = [((2 * x, y), el), ((2 * x + 1, y), el)]
  | el == 'O' = [((2 * x, y), '['), ((2 * x + 1, y), ']')]
  | otherwise = [((2 * x, y), el), ((2 * x + 1, y), '.')]

getDataFromFile_bigger :: FilePath -> IO (Grid, Movements, Position)
getDataFromFile_bigger fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
      (grid, rest) = break null fileLines
      movements_st = drop 1 rest

  let grid_pos = concat [map (\(x, i) -> ((x, y), i)) (zip [0 ..] line) | (y, line) <- (zip [0 ..] grid)]

  let bigger_grid = concatMap make_sq_bigger grid_pos

  let initial_pos = fst $ head $ filter (\(pos, el) -> el == '@') bigger_grid
  let hashMap = HashMap.fromList bigger_grid
  let movements = map map_movement (concat movements_st)
  return (hashMap, movements, initial_pos)

-------------------------------------------------------
--                       Movement                    --
-------------------------------------------------------

horizontal_movement :: Grid -> Position -> Movement -> (Grid, Position)
horizontal_movement grid pos movement = (end_grid, end_pos)
  where
    cur_element = (fromMaybe '#' (HashMap.lookup pos grid))
    new_pos = move pos movement

    next_element = (fromMaybe '#' (HashMap.lookup new_pos grid))

    consume_next_element :: Grid -> Char -> Grid
    consume_next_element grid '.' = HashMap.insert new_pos cur_element (HashMap.insert pos '.' grid)
    consume_next_element grid '#' = grid
    consume_next_element grid '[' = fst $ horizontal_movement grid new_pos movement
    consume_next_element grid ']' = fst $ horizontal_movement grid new_pos movement
    consume_next_element _ e = error ("Invalid element: " ++ [e])

    new_grid = consume_next_element grid next_element

    element_new_pos = HashMap.lookup new_pos new_grid
    end_grid =
      if element_new_pos == Just '.'
        then HashMap.insert new_pos cur_element (HashMap.insert pos '.' new_grid)
        else new_grid

    end_pos =
      if HashMap.lookup new_pos end_grid == Just cur_element
        then new_pos
        else pos

move_box_vertical :: Grid -> Grid -> Position -> Movement -> Bool -> Grid
move_box_vertical i_grid grid pos movement first =
  if pos == end_pos
    then i_grid
    else (if first then move_other_part end_grid else end_grid)
  where
    cur_element = (fromMaybe '#' (HashMap.lookup pos grid))
    new_pos = move pos movement

    next_element = (fromMaybe '#' (HashMap.lookup new_pos grid))

    consume_next_element :: Grid -> Char -> Grid
    consume_next_element grid '.' = HashMap.insert new_pos cur_element (HashMap.insert pos '.' grid)
    consume_next_element grid '#' = grid
    consume_next_element grid '[' = move_box_vertical i_grid grid new_pos movement True
    consume_next_element grid ']' = move_box_vertical i_grid grid new_pos movement True
    consume_next_element _ e = error ("Invalid element: " ++ [e])

    new_grid = consume_next_element grid next_element

    element_new_pos = HashMap.lookup new_pos new_grid
    end_grid =
      if element_new_pos == Just '.'
        then HashMap.insert new_pos cur_element (HashMap.insert pos '.' new_grid)
        else new_grid

    end_pos =
      if HashMap.lookup new_pos end_grid == Just cur_element
        then new_pos
        else pos

    move_other_part :: Grid -> Grid
    move_other_part _grid
      | cur_element == '[' = move_box_vertical i_grid _grid (x + 1, y) movement False
      | cur_element == ']' = move_box_vertical i_grid _grid (x - 1, y) movement False
      | otherwise = i_grid
      where
        (x, y) = pos

vertical_movement :: Grid -> Position -> Movement -> (Grid, Position)
vertical_movement grid pos movement = (end_grid, end_pos)
  where
    cur_element = (fromMaybe '#' (HashMap.lookup pos grid))
    new_pos = move pos movement

    next_element = (fromMaybe '#' (HashMap.lookup new_pos grid))

    consume_next_element :: Grid -> Char -> Grid
    consume_next_element grid '.' = HashMap.insert new_pos cur_element (HashMap.insert pos '.' grid)
    consume_next_element grid '#' = grid
    consume_next_element grid '[' = move_box_vertical grid grid new_pos movement True
    consume_next_element grid ']' = move_box_vertical grid grid new_pos movement True
    consume_next_element _ e = error ("Invalid element: " ++ [e])

    new_grid = consume_next_element grid next_element

    element_new_pos = HashMap.lookup new_pos new_grid
    end_grid =
      if element_new_pos == Just '.'
        then HashMap.insert new_pos cur_element (HashMap.insert pos '.' new_grid)
        else new_grid

    end_pos =
      if HashMap.lookup new_pos end_grid == Just cur_element
        then new_pos
        else pos

consume_movement_big :: Grid -> Position -> Movement -> (Grid, Position)
consume_movement_big grid pos (-1, 0) = horizontal_movement grid pos (-1, 0)
consume_movement_big grid pos (1, 0) = horizontal_movement grid pos (1, 0)
consume_movement_big grid pos (0, -1) = vertical_movement grid pos (0, -1)
consume_movement_big grid pos (0, 1) = vertical_movement grid pos (0, 1)

calculate_result_big :: Grid -> Int
calculate_result_big grid = sum $ map (\((x, y), _) -> x + (100 * y)) $ filter (\(pos, el) -> el == '[') (HashMap.toList grid)

part2 = do
  (grid, movements, initial_pos) <- getDataFromFile_bigger f_name

  let (new_grid, end_pos) = foldl (\(gr, pos) movement -> consume_movement_big gr pos movement) (grid, initial_pos) movements

  return $ calculate_result_big new_grid