exceedfibonumbers :: Int -> [Int]
exceedfibonumbers lim = [ x | x <- takeWhile ( <= lim) fibs, even x]
    where fibs = scanl (+) 0 (1:fibs)

