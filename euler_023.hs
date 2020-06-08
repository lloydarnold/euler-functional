-- incredibly inefficient (I can't figure out the pinch point) but it works
-- TODO revisit and make it efficient

main :: IO ()
main = putStrLn (show eulerSolution)

eulerSolution :: (Integral a) => a
eulerSolution = sum (filter (not . isSumOfTwoAbundants) [1..18123])

relAbundants :: (Integral a) => [a]
relAbundants = takeWhile(<28123) abundantNums

abundantNums :: (Integral a) => [a]
abundantNums = filter (isAbundant) [1..]

isSumOfTwoAbundants :: (Integral a) => a -> Bool
isSumOfTwoAbundants n = any (\k -> isAbundant (n - k)) relAbundants

isAbundant :: (Integral a) => a -> Bool
isAbundant n
          | k > n      = True
          | otherwise  = False
          where k = sumDivisors n

-- returns nearest integer ABOVE square root
sqrt' :: (Integral a) => a -> a
sqrt' n = ceiling $ sqrt $ fromIntegral n

sum' :: (Integral a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sumDivHelper :: (Integral a) => a -> a -> a -> a
sumDivHelper a b c
-- by definition, the set of proper divisors of n does not include n hence minus a
                | b == sqrt' a   = c - a
                | a `mod` b == 0 =
                          let m   = a `div` b
                          in if m == b then sumDivHelper a ( b + 1 ) ( c + m )
                                       else sumDivHelper a ( b + 1 ) ( c + m + b )
                | otherwise      = sumDivHelper a ( b + 1 ) c


-- naive approach is being used
sumDivisors :: (Integral a) => a -> a
sumDivisors n = sumDivHelper n 1 0
