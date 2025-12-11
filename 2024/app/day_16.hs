import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.PSQueue (Binding (..), PSQ)
import qualified Data.PSQueue as PSQ
import Debug.Trace (trace)

f_name :: FilePath
f_name = "./inputs/day_16/input_test.txt"

type Position = (Int, Int)

type CurrentPosition = (Position, Int)

type Grid = HashMap Position [Position]

get_neighbors :: Position -> [Position] -> [Position]
get_neighbors (x, y) nn = filter (\(x', y') -> (x', y') `elem` nn) [(x + 1, y), (x - 1, y), (x, y + 1), (x, y - 1)]

getDataFromFile :: FilePath -> IO (Grid, Position, Position)
getDataFromFile fileName =
  do
    contents <- readFile fileName
    let fileLines = lines contents

    let grid_pos = map fst $ filter (\(_, el) -> el /= '#') $ concat [map (\(x, i) -> ((x, y), i)) (zip [0 ..] line) | (y, line) <- (zip [0 ..] fileLines)]

    let grid = HashMap.fromList $ map (\(x, y) -> ((x, y), get_neighbors (x, y) grid_pos)) grid_pos

    let start_pos = (1, length fileLines - 2)
    let end_pos = ((length $ head fileLines) - 2, 1)

    return (grid, start_pos, end_pos)

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

-- Part 1

type Dist = HashMap Position Int

type PQ = PSQ CurrentPosition Int

get_key_value_of_pq :: PQ -> (CurrentPosition, Int)
get_key_value_of_pq pq =
  case PSQ.minView pq of
    Nothing -> (((-1, -1), -1), -1)
    Just (key PSQ.:-> priority, _) -> (key, priority)

get_new_cur_pos :: CurrentPosition -> Position -> CurrentPosition
get_new_cur_pos ((x, y), _) a@(x', y')
  | y - y' > 0 = (a, 0)
  | x - x' < 0 = (a, 1)
  | y - y' < 0 = (a, 2)
  | x - x' > 0 = (a, 3)

calculate_distance :: CurrentPosition -> CurrentPosition -> Int
calculate_distance (_, d) (_, d') = if abs (d - d') == 0 then 1 else 1001

-- Gpt-4o: Haskell priority queue
-- -- Example usage of PSQueue
-- main :: IO ()
-- main = do
--     --Create an empty priority queue
--     let pq = PSQ.empty

--     -- Insert elements into the priority queue
--     let pq1 = PSQ.insert "task1" 2 pq   -- Task 1 with priority 2
--     let pq2 = PSQ.insert "task2" 1 pq1 -- Task 2 with priority 1
--     let pq3 = PSQ.insert "task3" 3 pq2 -- Task 3 with priority 3

--     -- Show the priority queue
--     print pq3 -- PSQ [task2 :-> 1, task1 :-> 2, task3 :-> 3]

--     -- Get the element with the highest priority (lowest value)
--     let topElement = PSQ.findMin pq3
--     print topElement -- Just (task2 :-> 1)

--     -- Remove the top priority element
--     let pq4 = PSQ.deleteMin pq3
--     print pq4 -- PSQ [task1 :-> 2, task3 :-> 3]

--     -- Check if the queue is empty
--     print $ PSQ.null pq4 -- False

ds_loop :: Grid -> Dist -> PQ -> Position -> Int
ds_loop grid dist pq end =
  if (fst cur_pos) == (-1, -1)
    then -1
    else handle_loop cur_pos cur_dist
  where
    (cur_pos, cur_dist) = get_key_value_of_pq pq
    pq' = PSQ.deleteMin pq

    handle_loop :: CurrentPosition -> Int -> Int
    handle_loop c_pos c_dist =
      if (fst c_pos) == end
        then c_dist
        else
          ( if cur_min_dist /= -1 && c_dist > cur_min_dist
              then ds_loop grid dist pq' end
              else start_next_iter grid dist' pq' c_pos c_dist
          )
      where
        cur_min_dist = fromMaybe (-1) $ HashMap.lookup (fst c_pos) dist

        dist' =
          if cur_min_dist /= -1 && c_dist > cur_min_dist
            then dist
            else HashMap.insert (fst c_pos) c_dist dist

        start_next_iter :: Grid -> Dist -> PQ -> CurrentPosition -> Int -> Int
        start_next_iter grid dist pq cur_pos cur_dist = ds_loop grid ndist npq end
          where
            (ndist, npq) = loop_neighbors grid dist pq cur_pos cur_dist

    loop_neighbors :: Grid -> Dist -> PQ -> CurrentPosition -> Int -> (Dist, PQ)
    loop_neighbors grid dist pq cur_pos cur_dist =
      foldl (consume_neighbors cur_pos cur_dist) (dist, pq) (fromMaybe [] $ HashMap.lookup (fst cur_pos) grid)

    consume_neighbors :: CurrentPosition -> Int -> (Dist, PQ) -> Position -> (Dist, PQ)
    consume_neighbors cur_pos cur_dist (dist, pq) n =
      if n_dist == -1 || n_dist > new_dist
        then (dist, PSQ.insert new_cur_pos new_dist pq)
        else (dist, pq)
      where
        n_dist = fromMaybe (-1) $ HashMap.lookup n dist

        new_cur_pos = get_new_cur_pos cur_pos n
        new_dist = cur_dist + (calculate_distance cur_pos new_cur_pos)

dijkstra :: Grid -> CurrentPosition -> Position -> Int
dijkstra grid start end = ds_loop grid dist pq end
  where
    dist = HashMap.empty
    pq = PSQ.insert start 0 PSQ.empty

part1 = do
  (grid, start, end) <- getDataFromFile f_name

  return $ dijkstra grid (start, 1) end

-- Part 2

type Previous = HashMap Position [Position]

-- ds_loop_part2 :: Grid -> Dist -> PQ -> Previous -> Position -> Previous
-- ds_loop_part2 grid dist pq prev end =
--   if (fst cur_pos) == (-1, -1)
--     then prev
--     else handle_loop cur_pos cur_dist
--   where
--     (cur_pos, cur_dist) = get_key_value_of_pq pq
--     pq' = PSQ.deleteMin pq

--     handle_loop :: CurrentPosition -> Int -> Previous
--     handle_loop c_pos c_dist =
--       -- if end_dist /= -1 && c_dist > end_dist
--       --   then prev
--       --   else
--       ( if (cur_min_dist /= -1 && c_dist > cur_min_dist) || ((fst c_pos) == end)
--           then ds_loop_part2 grid dist pq' prev end
--           else start_next_iter grid dist' pq' prev c_pos c_dist
--       )
--       where
--         end_dist = fromMaybe (-1) $ HashMap.lookup end dist

--         cur_min_dist = fromMaybe (-1) $ HashMap.lookup (fst c_pos) dist

--         dist' =
--           if cur_min_dist /= -1 && c_dist >= cur_min_dist
--             then dist
--             else HashMap.insert (fst c_pos) c_dist dist

--         start_next_iter :: Grid -> Dist -> PQ -> Previous -> CurrentPosition -> Int -> Previous
--         start_next_iter grid dist pq prev cur_pos cur_dist = ds_loop_part2 grid ndist npq nprev end
--           where
--             (ndist, npq, nprev) = loop_neighbors grid dist pq prev cur_pos cur_dist

--     loop_neighbors :: Grid -> Dist -> PQ -> Previous -> CurrentPosition -> Int -> (Dist, PQ, Previous)
--     loop_neighbors grid dist pq prev cur_pos cur_dist =
--       foldl (consume_neighbors cur_pos cur_dist) (dist, pq, prev) (fromMaybe [] $ HashMap.lookup (fst cur_pos) grid)

--     consume_neighbors :: CurrentPosition -> Int -> (Dist, PQ, Previous) -> Position -> (Dist, PQ, Previous)
--     consume_neighbors cur_pos cur_dist (dist, pq, prev) n =
--       trace ("\n[consume_neighbors] n_dist=" ++ show n_dist ++ " new_dist=" ++ show new_dist ++ " cur_pos=" ++ show cur_pos ++ "ne=" ++ show n) $
--         if n_dist == -1 || n_dist > new_dist
--           then (dist, PSQ.insert new_cur_pos new_dist pq, HashMap.insert n ([fst cur_pos]) prev)
--           else
--             ( if n_dist == new_dist
--                 then error "HOOO" -- (dist, PSQ.insert new_cur_pos new_dist pq, HashMap.insert n ((fst cur_pos) : (fromMaybe [] $ HashMap.lookup n prev)) prev)
--                 else (dist, pq, prev)
--             )
--       where
--         n_dist = fromMaybe (-1) $ HashMap.lookup n dist

--         new_cur_pos = get_new_cur_pos cur_pos n
--         new_dist = cur_dist + (calculate_distance cur_pos new_cur_pos)

-- dijkstra_part2 :: Grid -> CurrentPosition -> Position -> Previous
-- dijkstra_part2 grid start end = ds_loop_part2 grid dist pq prev end
--   where
--     dist = HashMap.empty
--     prev = HashMap.empty
--     pq = PSQ.insert start 0 PSQ.empty

dijkstra_part2 :: Grid -> CurrentPosition -> Position -> Previous
dijkstra_part2 grid start end = ds_loop_part2 grid dist pq prev end
  where
    dist = HashMap.empty
    prev = HashMap.empty
    pq = PSQ.insert start 0 PSQ.empty

ds_loop_part2 :: Grid -> Dist -> PQ -> Previous -> Position -> Previous
ds_loop_part2 grid dist pq prev end =
  if (fst cur_pos) == (-1, -1)
    then prev
    else handle_loop cur_pos cur_dist
  where
    (cur_pos, cur_dist) = get_key_value_of_pq pq
    pq' = PSQ.deleteMin pq

    handle_loop :: CurrentPosition -> Int -> Previous
    handle_loop c_pos c_dist =
      if (cur_min_dist /= -1 && c_dist > cur_min_dist) || (fst c_pos == end)
        then ds_loop_part2 grid dist pq' prev end
        else start_next_iter grid dist' pq' prev c_pos c_dist
      where
        -- distance to the end (if stored)
        end_dist = fromMaybe (-1) $ HashMap.lookup end dist
        -- current node’s distance (if stored)
        cur_min_dist = fromMaybe (-1) $ HashMap.lookup (fst c_pos) dist

        -- If this node's distance is better than what we had, update it
        dist' =
          if cur_min_dist /= -1 && c_dist >= cur_min_dist
            then dist
            else HashMap.insert (fst c_pos) c_dist dist

        start_next_iter :: Grid -> Dist -> PQ -> Previous -> CurrentPosition -> Int -> Previous
        start_next_iter grid dist pq prev cur_pos cur_dist =
          ds_loop_part2 grid ndist npq nprev end
          where
            (ndist, npq, nprev) =
              loop_neighbors grid dist pq prev cur_pos cur_dist

    loop_neighbors ::
      Grid ->
      Dist ->
      PQ ->
      Previous ->
      CurrentPosition ->
      Int ->
      (Dist, PQ, Previous)
    loop_neighbors grid dist pq prev cur_pos cur_dist =
      foldl
        (consume_neighbors cur_pos cur_dist)
        (dist, pq, prev)
        (fromMaybe [] $ HashMap.lookup (fst cur_pos) grid)

    -- This is the critical function where we update 'prev' so that
    -- it *accumulates* all possible parents of a node (when distances tie).
    consume_neighbors ::
      CurrentPosition ->
      Int ->
      (Dist, PQ, Previous) ->
      Position ->
      (Dist, PQ, Previous)
    consume_neighbors (cPos, _) cDist (dist, pq, prev) n =
      -- If we found a strictly better distance, update distances and 'prev'
      if n_dist == -1 || n_dist > new_dist
        then
          ( HashMap.insert n new_dist dist,
            PSQ.insert (n, new_dist) new_dist pq,
            HashMap.insert n [cPos] prev
          )
        else
          -- If it's an *equal* distance, accumulate one more predecessor
          if n_dist == new_dist
            then
              ( dist,
                PSQ.insert (n, new_dist) new_dist pq,
                HashMap.insert n (cPos : fromMaybe [] (HashMap.lookup n prev)) prev
              )
            else
              (dist, pq, prev)
      where
        n_dist = fromMaybe (-1) $ HashMap.lookup n dist
        new_dist = cDist + calculate_distance (cPos, 0) (n, 0)

unique :: [Position] -> [Position]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

get_all_paths :: Previous -> Position -> [Position]
get_all_paths prev cur = cur_prevs ++ concatMap (get_all_paths prev) cur_prevs
  where
    cur_prevs = fromMaybe [] $ HashMap.lookup cur prev

part2 = do
  (grid, start, end) <- getDataFromFile f_name

  let prev = dijkstra_part2 grid (start, 1) end

  return $ length $ unique $ get_all_paths prev end

-- \
