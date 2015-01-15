

evaluatetwoorfive :: Int -> Int -> [Int] -> [Int]
evaluatetwoorfive _ _ [] = []
evaluatetwoorfive x y (z:zs) 
    | z `mod` x == 0 = z:evaluatetwoorfive x y zs
    | z `mod` y == 0 = z:evaluatetwoorfive x y zs
    | otherwise = evaluatetwoorfive x y zs
