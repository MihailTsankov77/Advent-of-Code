import Data.Bits
-- import Data.List

import qualified Data.IntMap.Strict as M
import Data.List.Split

type Prices = M.IntMap Int

main :: IO ()
main =
  do
    dataFileName <- getDataFileName
    text <- readFile dataFileName
    let codes = fmap read $ lines text
    -- print codes
    -- print $ fmap (followingSecret 2000) codes
    print $ part1 codes
    print $ part2 codes

part1, part2 :: [Int] -> Int
part1 codes = sum $ fmap (followingSecret 2000) codes
part2 codes = maximum $ M.elems mergedPriceValues
  where
    allPrices = fmap salePrices codes
    allPriceValues = fmap windowsAndPrices allPrices
    mergedPriceValues = M.unionsWith (+) allPriceValues

nextSecret, step1, step2, step3, prune :: Int -> Int
nextSecret = step3 . step2 . step1
step1 n = prune $ (n * 64) `mix` n
step2 n = prune $ (n `div` 32) `mix` n
step3 n = prune $ (n * 2048) `mix` n
prune n = n `mod` 16777216

mix :: Int -> Int -> Int
mix s n = s `xor` n

followingSecret :: Int -> Int -> Int
followingSecret n s = (!! n) $ iterate nextSecret s

salePrices :: Int -> [Int]
salePrices s = take 2001 $ fmap (`mod` 10) $ iterate nextSecret s

priceChanges :: [Int] -> [Int]
priceChanges ps = zipWith (-) (tail ps) ps

windows :: [Int] -> [Int]
windows = fmap encode . divvy 4 1

encode :: [Int] -> Int
encode xs = foldl' (\a n -> a * 20 + n) 0 xs

windowsAndPrices :: [Int] -> Prices
windowsAndPrices ps = foldl' (\m (w, p) -> M.insertWith (flip const) w p m) M.empty wPs
  where
    cs = priceChanges ps
    wPs = zip (windows cs) (drop 4 ps)
