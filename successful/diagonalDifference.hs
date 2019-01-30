diagonalDifference :: [[Int]] -> Int
diagonalDifference arr = abs ( foldl (+) 0 (map rowResult z) )
    where
        l = length arr
        z = zip [0..] (map (\x -> (l, x)) arr)

rowResult :: (Int, (Int, [Int])) -> Int
rowResult (i, (len, a)) = a!!i - a!!(len-i-1)