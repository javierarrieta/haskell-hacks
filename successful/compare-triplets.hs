compareTriplets :: [Int] -> [Int] -> [Int]
compareTriplets a b = toList(foldl f (0,0) (zip a b))

f :: (Int, Int) -> (Int, Int) -> (Int, Int)
f (suma, sumb) (a,b)
    | a > b = (suma+1, sumb)
    | a < b = (suma, sumb+1)
    | otherwise = (suma, sumb)

toList :: (Int, Int) -> [Int]
toList (a, b) = [a,b]