birthdayCakeCandles :: [Int] -> Int
birthdayCakeCandles ar = snd(traverseCandles ar)

traverseCandles :: [Int] -> (Int, Int)
traverseCandles ar = foldl myf (0,0) ar

myf :: (Int, Int) -> Int -> (Int, Int)
myf (max, count) a
    | a >  max = (a, 1)
    | a == max = (max, count+1)
    | otherwise = (max, count)