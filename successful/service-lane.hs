serviceLane :: [Int] -> [[Int]] -> [Int]
serviceLane width cases = map (w width) (map toTuple2 cases)


w :: [Int] -> (Int, Int) -> Int
w width (from, to) = minimum (slice from to width)

toTuple2 :: [Int] -> (Int, Int)
toTuple2 [a, b] = (a,b)

slice from to xs = take (to - from + 1) (drop from xs)