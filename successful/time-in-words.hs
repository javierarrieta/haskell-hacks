
timeInWords :: Int -> Int -> String
timeInWords h m
    | m == 0  = (inWord h) ++ " o' clock"
    | m == 1 = "one minute past " ++ (inWord h)
    | m == 15 = "quarter past " ++ (inWord h)
    | m == 30 = "half past " ++ (inWord h)
    | m == 45 = "quarter to " ++ (inWord (normalise(h+1)))
    | m < 30 = (inWord m) ++ " minutes past " ++ (inWord h)
    | m == 59 = "one minute to " ++ (inWord (normalise(h+1)))
    | otherwise = (inWord (60-m)) ++ " minutes to " ++ (inWord (normalise(h+1)))

normalise :: Int -> Int
normalise i
    | i == 13   = 1
    | otherwise = i

inWord :: Int -> String
inWord  1 = "one"
inWord  2 = "two"
inWord  3 = "three"
inWord  4 = "four"
inWord  5 = "five"
inWord  6 = "six"
inWord  7 = "seven"
inWord  8 = "eight"
inWord  9 = "nine"
inWord 10 = "ten"
inWord 11 = "eleven"
inWord 12 = "twelve"
inWord 13 = "thirteen"
inWord 14 = "fourteen"
inWord 15 = "fifteen"
inWord 16 = "sixteen"
inWord 17 = "seventeen"
inWord 18 = "eighteen"
inWord 19 = "nineteen"
inWord 20 = "twenty"
inWord 21 = "twenty one"
inWord 22 = "twenty two"
inWord 23 = "twenty three"
inWord 24 = "twenty four"
inWord 25 = "twenty five"
inWord 26 = "twenty six"
inWord 27 = "twenty seven"
inWord 28 = "twenty eight"
inWord 29 = "twenty nine"