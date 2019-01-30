flatlandSpaceStations n c = quot m 2
    where (_,m) = foldl dist (0,0) (sort (c ++ [n-1]))

dist :: (Int, Int) -> Int -> (Int, Int)
dist (last, m) next = (next, max m (next - last))

-- flatlandSpaceStations n c = maximum arr
--     where arr = map (maxdistance c) [0..n-1]

-- maxdistance :: [Int] -> Int -> Int
-- maxdistance sss city = minimum distances
--     where distances = map (\i -> abs (i - city)) sss