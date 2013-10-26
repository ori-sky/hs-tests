fib :: Int -> Int
fib n = fibGen 0 1 n

fibs :: [Integer]
fibs = 0 : 1 : [ a + b | (a, b) <- zip fibs (tail fibs)]

fibGen :: Int -> Int -> Int -> Int
fibGen a b n = case n of
    0 -> a
    n -> fibGen b (a + b) (n - 1)

fibPoly :: (Num a, Eq a, Num b, Eq b) => a -> b
fibPoly n = fibGenPoly 0 1 n

fibGenPoly :: (Num a, Eq a, Num b, Eq b) => b -> b -> a -> b
fibGenPoly a b 0 = a
fibGenPoly a b n = fibGenPoly b (a + b) (n - 1)
