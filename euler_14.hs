collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
        | even n = n:collatz (n `div` 2)
        | odd n  = n:collatz (3*n + 1)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Error, list empty"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)

eulerSolution :: Int
eulerSolution = maximum' (map length (map collatz [1..1000000]))
