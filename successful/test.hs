sayHello :: String -> IO()
sayHello x = putStrLn("Hello, " ++ x ++ "!")

triple x = x * 3

area r = pi * (r*r)