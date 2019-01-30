plusMinus :: [Int] -> IO ()
plusMinus arr = do
    print a
    print b
    print c
    where (a,b,c) = plusMinusF arr


plusMinusF :: [Int] -> (Float, Float, Float)
plusMinusF arr = tdiv (foldl tadd (0,0,0) arr) (fromIntegral(length arr))

tadd :: (Int, Int, Int) -> Int ->  (Int, Int, Int)
tadd (pos, neg, z) n
    | n > 0 = (pos+1, neg, z)
    | n < 0 = (pos, neg+1, z)
    | otherwise = (pos, neg, z+1)

tdiv :: (Int, Int, Int) -> Float -> (Float, Float, Float)
tdiv (a,b,c) l = ((fromIntegral a)/l, (fromIntegral b) / l, (fromIntegral c)/l)