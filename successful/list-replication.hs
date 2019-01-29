f :: Int -> [Int] -> [Int]
f n arr = foldl (g n) [] arr

g :: Int -> [Int] -> Int -> [Int]
g n prev i = prev ++ map (\_ -> i) [1..n]