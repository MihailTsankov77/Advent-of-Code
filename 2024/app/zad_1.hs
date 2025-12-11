repeat' :: Int -> Double -> [Double]
repeat' 0 _ = []
repeat' n x = x : repeat' (n - 1) x

drop_last :: [Double] -> [Double]
drop_last [] = []
drop_last [x] = []
drop_last (x : xs) = x : drop_last xs

multiplyEnds :: [Double] -> Double
multiplyEnds [] = 0
multiplyEnds [x] = x * x
multiplyEnds xs = head xs * last xs + multiplyEnds (tail (drop_last xs))

gen_number :: [Double] -> Double
gen_number (x : xs) = (multiplyEnds xs) / x

somos :: Int -> [Double]
somos 0 = []
somos k = (repeat' k 1) ++ [gen_number (take k (drop i (somos k))) | i <- [0 ..]]
