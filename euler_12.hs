import Data.List

-- returns nearest integer ABOVE square root
sqrt' :: (Integral a) => a -> a
sqrt' n = ceiling $ sqrt $ fromIntegral n

cntDivHelper :: (Integral a) => a -> a -> a -> a
cntDivHelper a b c
                | b > sqrt' a    = c
                | a `mod` b == 0 = if a `div` b == b then cntDivHelper a ( b + 1 ) ( c + 1)
                                                     else cntDivHelper a ( b + 1 ) ( c + 2 )
                | otherwise      = cntDivHelper a ( b + 1 ) c

-- naive approach is being used -- ugly time complexity of O(sqrt n)
countDivisors :: (Integral a) => a -> a
countDivisors n = cntDivHelper n 1 0

triangle :: (Integral a) => a -> a
triangle n = n * (n + 1) `div` 2  -- div used here to avoid type error

triNums :: (Integral a) => [a]
triNums = map triangle [1..]

eulerSolution :: (Integral a) => Maybe a
eulerSolution = find ( \x -> (countDivisors x) > 500 ) triNums
