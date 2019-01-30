miniMaxSumF :: [Int] -> (Int, Int)
miniMaxSumF arr = ( minimum values, maximum values)
    where s = sum arr
          values = map (s-) arr