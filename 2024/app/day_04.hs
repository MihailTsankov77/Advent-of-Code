-- Source: https://williamyaoh.com/posts/2019-04-11-cheatsheet-to-regexes-in-haskell.html
import Text.Regex.TDFA
import Text.Regex.TDFA.Text ()
-- GPT-4o : Can I transpose a matrix of string in Haskell  [String] -> [String]; ["abc", "abc"] => ["aa", "bb", "cc"]
import Data.List (transpose)

f_name :: FilePath
f_name = "./inputs/day_04/input.txt"

getDataFromFile :: FilePath -> IO [String]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  return fileLines

-- Part 1

xmas:: String
xmas = "XMAS"

reverse_xmas:: String
reverse_xmas = "SAMX"

count_xmas:: String -> Int
count_xmas row = 
  length (getAllTextMatches(row =~ xmas) :: [String])
  + length (getAllTextMatches(row =~ reverse_xmas) :: [String])


get_diagonal:: [String] -> String
get_diagonal [] = []
get_diagonal (x:xs) 
  | x == [] = []
  | otherwise = head x : get_diagonal (map tail xs)

get_vertical_diagonals:: [String] -> [String]
get_vertical_diagonals [] = []
get_vertical_diagonals xss = get_diagonal xss : get_vertical_diagonals (tail xss)

get_horizontal_diagonals:: [String] -> [String]
get_horizontal_diagonals [] = []
get_horizontal_diagonals xss 
  | null (head xss) = []
  | otherwise = get_diagonal xss : get_horizontal_diagonals (map tail xss)

get_matrix_diagonals:: [String] -> [String]
get_matrix_diagonals [] = []
get_matrix_diagonals xss =  get_vertical_diagonals xss ++ get_horizontal_diagonals (map tail xss)

part1 = do
  filedata <- getDataFromFile f_name
  let horizontal =sum(map count_xmas filedata)
  let vertical = sum(map count_xmas (transpose filedata))
  let diagonal_1 = sum(map count_xmas (get_matrix_diagonals filedata))
  let diagonal_2 = sum(map count_xmas (get_matrix_diagonals (map reverse filedata)))
  return $ horizontal + vertical + diagonal_1 + diagonal_2



-- Part 2
mas:: String
mas = "MAS"

reverse_mas:: String
reverse_mas = "SAM"

find_mas_indexes:: String -> [Int]
find_mas_indexes row = 
  map (\(x, _) -> x +1) (getAllMatches (row =~ mas) :: [(Int, Int)])
  ++ map (\(x, _) -> x +1) (getAllMatches (row =~ reverse_mas) :: [(Int, Int)])

get_position:: Int-> Int->Int -> (Int, Int)
get_position offX offY i = (offX + i, offY + i)

get_matrix_a_pos:: [String] -> [(Int, Int)]
get_matrix_a_pos [] = []
get_matrix_a_pos xss = vert_a_indexes ++ hor_a_indexes
  where
    vertical_diagonals = get_vertical_diagonals xss
    horizontal_diagonals = get_horizontal_diagonals (map tail xss)

    vert_a_indexes = concat [map (\x -> get_position 0 offY x) (find_mas_indexes row) | (row, offY) <- (zip vertical_diagonals [0..])]
    hor_a_indexes = concat [map (\x -> get_position offX 0 x) (find_mas_indexes row) | (row, offX) <- (zip horizontal_diagonals [1..])]



get_position_rev:: Int-> Int -> Int -> Int -> (Int, Int)
get_position_rev offX offY i len = (len - 1 - (offX + i), offY + i)

--- ["S.S....", ".A.A...", "M.M.M.M", ".A.A.A.", "S.S.S.S"]

--- S . S . . . . 
--- . a . A . . . 
--- M . M . M . M 
--- . a . a . a . 
--- S . S . S . S

-- (1 1) (1 3) (3, 3)

-- get_rev_matrix_a_pos:: [String] -> [(Int, Int)]
get_rev_matrix_a_pos [] = []
get_rev_matrix_a_pos xss = vert_a_indexes ++ hor_a_indexes
  where
    vertical_diagonals = get_vertical_diagonals xss
    horizontal_diagonals = get_horizontal_diagonals (map tail xss)

    len = length (head xss)

    vert_a_indexes = concat [map (\x -> get_position_rev 0 offY x len) (find_mas_indexes row) | (row, offY) <- (zip vertical_diagonals [0..])]
    hor_a_indexes = concat [map (\x -> get_position_rev offX 0 x len) (find_mas_indexes row) | (row, offX) <- (zip horizontal_diagonals [1..])]


part2 = do
  filedata <- getDataFromFile f_name
  let diagonal_1 = get_matrix_a_pos filedata
  let diagonal_2 = get_rev_matrix_a_pos (map reverse filedata)
  return $ length $ filter (`elem` diagonal_2) diagonal_1


--   return $ diagonal_1 ++ diagonal_2