import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.Hashable (Hashable)
import Data.List (iterate)

f_name :: FilePath
f_name = "./inputs/day_09/input.txt"

-- Gpt-4o: split char by char and parse the char to ints
splitCharsToInts :: String -> [Int]
splitCharsToInts = map (read . (: []))

getDataFromFile :: FilePath -> IO [Int]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLine = head $ lines contents

  return $ splitCharsToInts fileLine

fromMaybe :: a -> Maybe a -> a
fromMaybe _ (Just x) = x
fromMaybe d Nothing = d

lookup_with_default :: (Eq k, Hashable k) => v -> k -> HashMap k v -> v
lookup_with_default d k m = fromMaybe d (HashMap.lookup k m)

-- Part 1

type Sequences = HashMap Int Int

build_sequence_init :: [Int] -> (Sequences, Int)
build_sequence_init xs = build_sequence xs HashMap.empty 0 0 True

build_sequence :: [Int] -> Sequences -> Int -> Int -> Bool -> (Sequences, Int)
build_sequence [] seq _ pos _ = (seq, pos - 1)
build_sequence (x : xs) seq id pos isNum = build_sequence xs seq' id' pos' (not isNum)
  where
    real_id = if isNum then id else -1
    id' = if isNum then id + 1 else id
    pos' = pos + x
    seq' = foldl (\s p -> HashMap.insert p real_id s) seq [pos .. pos' - 1]

find_last_non_neg :: Int -> Sequences -> Int
find_last_non_neg i map = if cur /= -1 then i else find_last_non_neg (i - 1) map
  where
    cur = lookup_with_default (-1) i map

compact_sequence :: Sequences -> Int -> Int -> Sequences
compact_sequence seq start end = if start >= end' then seq else compact_sequence seq' (start + 1) final_end
  where
    cur = lookup_with_default (-1) start seq
    end' = find_last_non_neg end seq
    last_non_neg = lookup_with_default (-1) end' seq
    seq' = if cur /= -1 then seq else HashMap.insert end' (-1) (HashMap.insert start last_non_neg seq)
    final_end = if cur /= -1 then end' else (end' - 1)

build_arr_from_map :: Int -> Sequences -> [Int]
build_arr_from_map end seqMap = map (\i -> lookup_with_default 0 i seqMap) [0 .. end]

part1 = do
  bytes <- getDataFromFile f_name
  let (seq, end) = build_sequence_init bytes

  let compacted_seq = compact_sequence seq 0 end

  let arr = build_arr_from_map end compacted_seq
  return $ sum $ filter (> 0) $ map (\(x, y) -> x * y) $ zip arr [0 ..]

-- Part 2

type SequencesBlocks = HashMap Int (Int, Int)

build_sequence_blocks_init :: [Int] -> (SequencesBlocks, Int)
build_sequence_blocks_init xs = build_sequence_blocks xs HashMap.empty 0 0 True

build_sequence_blocks :: [Int] -> SequencesBlocks -> Int -> Int -> Bool -> (SequencesBlocks, Int)
build_sequence_blocks [] seq _ pos _ = (seq, pos - 1)
build_sequence_blocks (x : xs) seq id pos isNum = build_sequence_blocks xs seq' id' pos' (not isNum)
  where
    real_id = if isNum then id else -1
    id' = if isNum then id + 1 else id
    pos' = pos + x
    seq' = foldl (\s p -> HashMap.insert p (real_id, x) s) seq [pos .. pos' - 1]

find_last_non_neg_part2 :: Int -> SequencesBlocks -> Int
find_last_non_neg_part2 i map = if cur /= -1 then i else find_last_non_neg_part2 (i - 1) map
  where
    (cur, len) = lookup_with_default (-1, -1) i map

--- Fix Grid ---
fix_back_part_grid_init :: Int -> SequencesBlocks -> (SequencesBlocks, (Int, Int))
fix_back_part_grid_init end seq = (fix_back_part_grid end len seq, (cur, len))
  where
    (cur, len) = lookup_with_default (-1, -1) end seq

fix_back_part_grid :: Int -> Int -> SequencesBlocks -> SequencesBlocks
fix_back_part_grid pos len seq = if len == 0 then seq else fix_back_part_grid (pos - 1) (len - 1) (HashMap.insert pos (-1, -1) seq)

fix_front_part_grid :: Int -> Int -> Int -> Int -> SequencesBlocks -> SequencesBlocks
fix_front_part_grid cur_pos el i len seq = if cur /= -1 then seq else fix_front_part_grid (cur_pos + 1) el (i + 1) len seq'
  where
    (cur, cur_len) = lookup_with_default (-1, -1) cur_pos seq

    new_elem = if i >= len then -1 else el
    new_len = if i >= len then cur_len - len else len

    seq' = HashMap.insert cur_pos (new_elem, new_len) seq

fix_grid :: Int -> Int -> SequencesBlocks -> SequencesBlocks
fix_grid start end seq = fix_front_part_grid start el 0 len seq'
  where
    (seq', (el, len)) = fix_back_part_grid_init end seq

--- Fix Grid ---

build_arr_from_map_part2 :: Int -> SequencesBlocks -> [Int]
build_arr_from_map_part2 end seq = map (\i -> fst $ lookup_with_default (0, 0) i seq) [0 .. end]

find_start_index :: Int -> Int -> SequencesBlocks -> Int
find_start_index i end_len seq =
  if cur_len == -1
    then -1
    else
      (if cur == -1 && cur_len >= end_len then i else find_start_index (i + 1) end_len seq)
  where
    (cur, cur_len) = lookup_with_default (-1, -1) i seq

maybe_move_block :: Int -> Int -> Int -> SequencesBlocks -> SequencesBlocks
maybe_move_block end_pos el end_len seq = if start_index == -1 || start_index >= end_pos then seq else fix_grid start_index end_pos seq
  where
    start_index = find_start_index 0 end_len seq

compact_sequence_part2 :: SequencesBlocks -> Int -> SequencesBlocks
compact_sequence_part2 seq end =
  if end <= 1
    then seq
    else
      ( if cur == -1
          then
            compact_sequence_part2 seq (find_last_non_neg_part2 end seq)
          else compact_sequence_part2 (maybe_move_block end cur len seq) (end - len)
      )
  where
    (cur, len) = lookup_with_default (-1, -1) end seq

part2 = do
  bytes <- getDataFromFile f_name
  let (seq, end) = build_sequence_blocks_init bytes

  let compacted_seq = compact_sequence_part2 seq end

  let arr = build_arr_from_map_part2 end compacted_seq

  return $ sum $ filter (> 0) $ map (\(x, y) -> x * y) $ zip arr [0 ..]
