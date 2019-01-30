workbook n k arr = sum(map (\(a,b) -> if contains b a then 1 else 0) pages)
    where pages = zip [1..] (foldl (f k) [] arr)

f :: Int -> [(Int, Int)] -> Int -> [(Int, Int)]
f k arr i = arr ++ (pagesFor k i)

pagesFor k i = (map (\x -> (((x-1)*k+1), x*k)) [1..fullPages]) ++ maybeLastPage
    where 
        (fullPages, remainder) = quotRem i k
        maybeLastPage = if remainder == 0 then [] else [(k*fullPages+1, k*fullPages+remainder)]
contains :: (Int, Int) -> Int -> Bool
contains (from, to) i = i >= from && i <= to