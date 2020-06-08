import Data.Ratio

{-- A better solution, that uses the maths of Farey sequences
Specifically, uses the property that each new term is the mediant of the
previous adjacant pair --}

target :: Ratio Integer
target = 3%7
limit :: Integer
limit = 1000000

mediant :: Ratio Integer -> Ratio Integer -> Ratio Integer
mediant a b = (numerator a + numerator b) % ( denominator a + denominator b)

eulerSolution :: Ratio Integer
eulerSolution = driver (0%1) (1%1) (0%1)

driver :: Ratio Integer -> Ratio Integer -> Ratio Integer -> Ratio Integer
driver l r p
        | a > target  = driver l a a
        | a == target = driver l a p
        | otherwise   = if denominator a < limit then driver a r a
                                                 else p
        where a = mediant l r
