import Data.Foldable (forM_)
import qualified Data.Set as Set
import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_14/input.txt"

splitInt :: String -> [String]
splitInt x = getAllTextMatches (x =~ "-?[0-9]+") :: [String]

parseLine :: String -> (Int, Int, Int, Int)
parseLine line = (read a, read b, read c, read d) where [a, b, c, d] = splitInt line

getDataFromFile :: FilePath -> IO [(Int, Int, Int, Int)]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  return $ map parseLine fileLines

-- Part 1
simulate_robot :: (Int, Int, Int, Int) -> Int -> Int -> Int -> (Int, Int)
simulate_robot (x, y, dx, dy) seconds width height = (x'', y'')
  where
    x' = x + dx * seconds
    y' = y + dy * seconds
    dx' = dx
    dy' = dy
    x'' = x' `mod` width
    y'' = y' `mod` height

part1 = do
  robots <- getDataFromFile f_name
  let width = 101
  let height = 103
  let last_pos = map (\r -> simulate_robot r 100 width height) robots
  let robots_in_sector_A = length $ filter (\(x, y) -> x < width `div` 2 && y < height `div` 2) last_pos
  let robots_in_sector_B = length $ filter (\(x, y) -> x > width `div` 2 && y < height `div` 2) last_pos
  let robots_in_sector_C = length $ filter (\(x, y) -> x < width `div` 2 && y > height `div` 2) last_pos
  let robots_in_sector_D = length $ filter (\(x, y) -> x > width `div` 2 && y > height `div` 2) last_pos
  return $ product [robots_in_sector_A, robots_in_sector_B, robots_in_sector_C, robots_in_sector_D]

-- Part 2

unique :: (Eq a) => [a] -> [a]
unique [] = []
unique (x : xs) = x : unique (filter (/= x) xs)

test_diff_seconds :: [(Int, Int, Int, Int)] -> Int -> Int -> Int -> ([(Int, Int)], Int)
test_diff_seconds robots seconds border limit =
  if seconds > limit
    then ([], -1)
    else
      (if uniq_robots < border then (res, uniq_robots) else test_diff_seconds robots (seconds + 1) border limit)
  where
    res = map (\r -> simulate_robot r seconds 101 103) robots
    uniq_robots = length $ unique $ res

-- Gpt-4o: You get an array of (int, int)
-- print a grid with width 101 and height 103 where everything is a . expect the position in the array they should be #
printGrid :: [(Int, Int)] -> IO ()
printGrid coords = do
  let width = 101
      height = 103
      positionsSet = Set.fromList coords

  forM_ [0 .. height - 1] $ \y -> do
    forM_ [0 .. width - 1] $ \x -> do
      if (x, y) `Set.member` positionsSet
        then putChar '#'
        else putChar '.'
    putChar '\n'

-- Solution thanks to Reddit.
-- Idea check the frames where there are small number of unique robots -> they are creating a picture so we need a clear board
-- I am checking in batches because my comp is crashing :))))
part2' start = do
  robots <- getDataFromFile f_name

  let (pos, seconds) = test_diff_seconds robots start 10000 (start + 10000)

  if seconds == -1
    then return (-1)
    else do
      printGrid pos
      return seconds

-- Tested with limit 500
-- 0 miss 0

-- Tested with limit 450
-- 0 - miss 775 , 10000, miss 11178, 21179 - miss 21581 (Note: it is the same as 11178 wtf they are repeating and the same as 775) -- the answer is not here

-- Tested with limit 350
-- 0, 2000, 4000, 6000, 8000, 10000, 12000, 14000, 16000,....50000

-- Tested with limit 400
-- 0 - 10000

-- Tested with limit 470
-- too much

-- Tested with limit 460

-- This doesn't work for me ;(
-- but I found a patter in the frames that every 101 seconds there is a similar frame
-- and then I build a function that will iterate over the frames and print the grid and I checked them...

part2 start_iter = do
  robots <- getDataFromFile f_name

  let step = 101
  let start = 68
  let iterations = start_iter + 4
  let endSec = start * start_iter + (step * (iterations - 1))

  -- GPT-o1 Iterate in steps of 103: e.g. if start=12, we get 12, 115, 218, ...
  forM_ [start, start + step .. endSec] $ \sec -> do
    let coords = map (\r -> simulate_robot r sec 101 103) robots
    let uniqueCount = length (unique coords)

    putStrLn ("Iteration " ++ show sec)
    printGrid coords
    putStrLn ""

-- found it 7138