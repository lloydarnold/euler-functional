import Data.Ratio

limit :: Integer
limit = 8

eulerSolution :: Ratio Integer
eulerSolution = ordFracs !! ( ( length $ takeWhile (< 3%7) ordFracs )- 1)

ordFracs :: [Ratio Integer]
ordFracs = quickSort listFracs

listFracs :: [Ratio Integer]
listFracs = [n % d | d <- [1..limit], n <- [1..d], hcf d n == 1]

hcf :: Integer -> Integer -> Integer
hcf a 0 = a
hcf a b = hcf b (a `mod` b)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x:xs) =
    let smallerSorted = quickSort (filter (<=x) xs)
        biggerSorted = quickSort (filter (>x) xs)
    in  smallerSorted ++ [x] ++ biggerSorted
