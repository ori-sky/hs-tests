length'recursive :: (Num b) => [a] -> b
length'recursive [] = 0
length'recursive (_:xs) = 1 + length'recursive xs

length' :: (Num b) => [a] -> b
length' xs = sum [1 | _ <- xs]
