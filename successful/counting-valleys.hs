
charToSlope :: Char -> Int
charToSlope c
  | c == 'D'  = (-1)
  | otherwise = 1


elevationF :: ([Int], Int) -> Int -> ([Int], Int)
elevationF (l,el) slope = (l ++ [slope + el], slope + el)

elevations :: [Int] -> [Int]
elevations a = fst(foldl elevationF ([], 0) a)

tupleWithPrevious :: ([(Int, Int)], Int) -> Int -> ([(Int, Int)], Int)
tupleWithPrevious (a, prev) cur = (a ++ [(prev, cur)], cur)

prevWithCur :: [Int] -> [(Int, Int)]
prevWithCur a = fst(foldl tupleWithPrevious ([], 0) a)

count   :: Eq a => a -> [a] -> Int
count x =  length . filter (==x)

countingValleys :: Int -> String -> Int
countingValleys n s = count (-1, 0) (prevWithCur (elevations(map charToSlope s)))

