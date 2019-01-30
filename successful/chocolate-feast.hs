
chocolateFeast :: Int -> Int -> Int -> Int
chocolateFeast n c m = reduceChocolates m chocs chocs
    where chocs = quot n c

reduceChocolates :: Int -> Int -> Int -> Int
reduceChocolates w2c chocolates wrappers
    | wrappers < w2c = chocolates
    | otherwise = reduceChocolates w2c (chocolates+chocs) (wra+chocs)
        where (chocs, wra) = quotRem wrappers w2c