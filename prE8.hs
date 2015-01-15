import Data.Char
import Data.List

main = do 
    contents <- readFile "numbersprE8.txt"
    print 
        . maximum . map product 
        . foldr (zipWith (:)) (repeat [])
        . take 13 . tails . map (fromIntegral . digitToInt) . concat 
        . lines $ contents
    
