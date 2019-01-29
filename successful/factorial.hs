import Control.Arrow

factorial :: Integer ->  Either String Integer
factorial n
  | n < 0       = Left "Invalid argument, factorial can only be applied to positive numbers"
  | n == 0      = Right 1
  | otherwise   = right (* n) (factorial (n - 1))