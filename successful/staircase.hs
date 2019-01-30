import Data.List

staircase :: Int -> IO ()
staircase n = mapM_ putStrLn (staircaseF n)


staircaseF :: Int -> [String]
staircaseF n = map (stepF n) [1..n]

stepF :: Int -> Int -> String
stepF n i = (replicate (n-i) ' ') ++ (replicate (i) '#')