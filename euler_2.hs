problem_2 = sum (filter even (filter (<4000000) (firstNFib 100000)))

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

firstNFib :: Int -> [Integer]
firstNFib n = take n fibs
