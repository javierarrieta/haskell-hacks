solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r a b = toList $ tupleSum $ map wc [l',l'+step..r']
    where step = 0.0001 :: Double
          l' = fromIntegral(l) :: Double
          r' = fromIntegral(r) :: Double
          wedge x = area x (step + x) (f a b x) (f a b $ (step + x))
          cyl x = pi * ((f a b x)^2 + (f a b (step+x))^2) / 2 * step
          wc x = (wedge x, cyl x)
          toList (qq,ww) = [qq, ww]

f :: [Int] -> [Int] -> Double -> Double
f a b x = sum $ map (g x) z
    where z = zip a b

g :: Double -> (Int, Int) -> Double
g x (a, b) = x^^b * fromIntegral(a)

area :: Double -> Double -> Double -> Double -> Double
area x x' y y' = (x' - x) * (y + y') / 2

tupleSum :: [(Double, Double)] -> (Double, Double)
tupleSum a = foldr ts (0,0) a
    where ts (a1,b1) (a2,b2) = (a1+a2, b1+b2)