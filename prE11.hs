import System.IO
import Data.List

directions = ["East", "South", "Southeast", "Southwest"]

main = do 
    content <- readFile "number.txt"
    print $ maximum . comparebestmatch content $ directions

comparebestmatch :: String -> [String] -> [Int]
comparebestmatch x [] = []
comparebestmatch cont (x:xs)
    | x == "East" = (maximum . map maximum . map prodvalue $ grid) : comparebestmatch cont xs
    | x == "South" = (maximum . map maximum . map prodvalue . 
                        transpose $ grid) : comparebestmatch cont xs
    | x == "Southeast" = (maximum . map maximum . map prodvalue $ (diagonalgrid grid) ++ (diagonalgrid . transpose $ grid)) : comparebestmatch cont xs
    | x == "Southwest" = (maximum . map maximum . map prodvalue $ (diagonalsouthgrid grid) ++ (diagonalsouthgrid . transpose $ grid)) : comparebestmatch cont xs
    | otherwise = comparebestmatch cont xs 
        where grid = map convertinnum . readarrarr $ cont

readarrarr :: String -> [[String]] 
readarrarr = fmap words . lines 

convertinnum :: [String] -> [Int]
convertinnum xs = [ read x | x <- xs]

prodvalue :: [Int] -> [Int]
prodvalue = map (product . take 4) . tails


diagonalgrid :: [[Int]] -> [[Int]]
diagonalgrid grid = map (diagonalrow grid) [0 .. (length grid)]

diagonalrow :: [[Int]] -> Int -> [Int]
diagonalrow grid offset = zipWith (!!) grid [offset .. max]
    where
        len = length $ grid!!0
        max = len - 1
        
diagonalsouthgrid :: [[Int]] -> [[Int]]
diagonalsouthgrid grid = map (diagonalsouthwestrow grid) $ reverse [0 .. (length grid - 1)]

diagonalsouthwestrow :: [[Int]] -> Int -> [Int] 
diagonalsouthwestrow grid offset = zipWith (!!) grid countposition
    where
        countposition = reverse [0.. offset]
 
