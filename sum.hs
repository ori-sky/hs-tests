sumList :: [Int] -> Int
sumList l = (l !! 0) + sum (tail l)
