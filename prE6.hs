

squareEuler :: Int -> Int
squareEuler n = n * n

squareOfList :: [Int] -> Int 
squareOfList xs = squareEuler $ sum xs

squareindividually :: [Int] -> [Int]
squareindividually = map squareEuler 
