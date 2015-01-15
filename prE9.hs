pythagoreanTriplet :: Int -> [[Int]]
pythagoreanTriplet l = [[a,b,c] | c <- [1..limit], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a + b + c == l]
     where limit = l `div` 2
