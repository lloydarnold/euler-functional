problem_2 :: Integer
problem_2 = sum (filter even $ takeWhile (<4000000) fibs)

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)
