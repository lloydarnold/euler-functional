import Data.Ratio

{-- A better solution, that uses the maths of Farey sequences
Specifically, uses the property that each new term is the mediant of the
previous adjacant pair --}

limit :: Integer
limit = 1000000

mediant :: Ratio Integer -> Ratio Integer -> Ratio Integer
mediant a b = (numerator a + numerator b) % ( denominator a + denominator b)
