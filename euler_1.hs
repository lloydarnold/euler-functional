genList :: Int -> [Int]
genList n = [a | a <- [1..n], a `mod` 3 == 0 || a `mod` 5 == 0]

sumList :: [Int] -> Int
sumList (x:[]) = x
sumList (x:xs) = x + sumList xs

main :: IO ()
main = print(sumList (genList 1000) )
