import Data.List

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
    let smallerSorted = quicksort (filter (<=x) xs)
        biggerSorted = quicksort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted


powers :: (Integral a) => a -> a -> a
powers 0 _ = 0
powers _ 0 = 1
powers n i
       | even i = (powers n (i `div` 2)) * (powers n (i `div` 2))
       | odd i  = n * powers n (i-1)


genList :: (Integral a) => a -> a -> [a]
genList a b = [powers x y | x <- [2..a], y <- [2..b]]

eulerSolution :: Int
eulerSolution = length (group (quicksort (genList 100 100)))
