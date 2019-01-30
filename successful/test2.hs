 
factorial :: Integer -> Either String Integer
factorial n
  | n < 0     = Left "Cannot get the factorial of a negative number: "
  | n == 0    = Right 1
  | otherwise = mapRight (factorial (n - 1)) (\a -> a * n)