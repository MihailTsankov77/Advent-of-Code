import Text.Regex.TDFA

f_name :: FilePath
f_name = "./inputs/day_13/input.txt"

splitInt :: String -> [String]
splitInt x = getAllTextMatches (x =~ "[0-9]+") :: [String]

groupLines :: [String] -> [[String]]
groupLines [] = []
groupLines xs = group : groupLines (drop 1 rest)
  where
    (group, rest) = break null xs

getDataFromFile :: FilePath -> IO [[(Integer, Integer)]]
getDataFromFile fileName = do
  contents <- readFile fileName
  let fileLines = lines contents
  let groupedLines = groupLines fileLines
  let parseLine line = (read a, read b) where [a, b] = splitInt line
  return $ map (map parseLine) groupedLines

-- Part 1

compute' :: (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer) -> (Integer, Integer)
compute' (aX, aY) (bX, bY) (pX, pY) = if null all_solutions then (0, 0) else head all_solutions
  where
    maxA = min (pX `div` aX) (pY `div` aY)
    maxB = min (pX `div` bX) (pY `div` bY)
    all_solutions :: [(Integer, Integer)]
    all_solutions =
      [ (n, m)
        | m <- [0 .. maxB],
          let n = (pX - m * bX) `div` aX,
          n * aX + m * bX == pX,
          n * aY + m * bY == pY
      ]

calculate :: (Integer, Integer) -> Integer
calculate (a, b) = a * 3 + b * 1

compute :: [(Integer, Integer)] -> Integer
compute xs = calculate $ compute' (xs !! 0) (xs !! 1) (xs !! 2)

part1 = do
  levels <- getDataFromFile f_name
  return $ sum $ map compute levels

-- Part 2

computeTheSmartWay :: [(Integer, Integer)] -> Integer
computeTheSmartWay (a : b : (pX, pY) : _) = calculate $ comp a b (pX + 10000000000000, pY + 10000000000000)
  where
    -- GPT-o1:
    -- I have two pairs of numbers (aX, aY) (bX,bY) and I have a target (x,y)
    -- I want to find a way to check if ax * number + bx*number = x and the same goes for y
    -- I want this to be as fast as possible with arithmetic operations
    -- double det = aX*bY - aY*bX;
    -- if (det != 0.0) {
    -- double M = (x*bY - y*bX) / det;
    -- double N = (aX*y - aY*x) / det;
    -- }
    comp (aX, aY) (bX, bY) (pX, pY) = if a > 0 && b > 0 then (a, b) else (0, 0)
      where
        det = aX * bY - aY * bX
        a = if det /= 0 && (pX * bY - pY * bX) `rem` det == 0 then (pX * bY - pY * bX) `div` det else 0
        b = if det /= 0 && (-pX * aY + pY * aX) `rem` det == 0 then (-pX * aY + pY * aX) `div` det else 0
computeTheSmartWay _ = 0

part2 = do
  levels <- getDataFromFile f_name
  return $ sum $ map computeTheSmartWay levels
