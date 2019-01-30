timeConversion :: String -> String
timeConversion twelveHrFmt = tfhf ++ ":" ++ mm ++ ":" ++ ss
    where mm = slice 3 4 twelveHrFmt
          ss = slice 6 7 twelveHrFmt
          hh = slice 0 1 twelveHrFmt
          am1pm = slice 8 9 twelveHrFmt
          attempt = calc24HrFmt hh am1pm
          tfhf 
            | attempt == "24" = "00"
            | otherwise       = attempt


calc24HrFmt hh am1pm
    | am1pm == "AM" && hh == "12" = "00"
    | am1pm == "AM" = hh
    | am1pm == "PM" && hh == "12" = "12"
    | otherwise     = show ((read hh :: Integer) + 12)
slice from to xs = take (to - from + 1) (drop from xs)