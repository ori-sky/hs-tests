sumList :: [Int] -> Int
sumList l = if length l == 0 then 0 else head l + sumList (tail l)
