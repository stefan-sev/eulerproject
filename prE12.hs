import Data.List

trianglnum = scanl (+) 0 [1..]

divisors :: Integer -> [Integer]
divisors n = [x | x <- [1..n], n `rem` x == 0]  

solutiontrianglnum :: [[Integer]] -> [[Integer]]
solutiontrianglnum (x:xs)
    | length x >= 10 = x : []
    | length x < 10 = x : solutiontrianglnum xs
    | otherwise = []


solution = head $ filter ((> 500) . nDivisors) triangleNumbers
  where nDivisors n = product $ map ((+1) . length) (group (primeFactor n))    
        triangleNumbers = scanl1 (+) [1..]

primes = 2 : filter (null . tail . primeFactor) [3,5..]

primeFactor n = factor n primes
    where       
        factor n (p:ps)   
            | p*p > n         = [n]     
            | n `mod` p == 0  = p : factor (n `div` p) (p:ps)  
            | otherwise       = factor n ps

