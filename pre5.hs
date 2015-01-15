smallestMultiple :: Int -> Int
smallestMultiple n = foldr1 lcm [1..n] 
