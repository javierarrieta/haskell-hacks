myExp :: Int -> Double -> Double
myExp n x = sum $ map r [0..n-1]
    where r i = (x^i) / fromIntegral(factorial i)
          factorial t = product [1..t]