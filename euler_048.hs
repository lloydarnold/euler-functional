powers :: (Integral a) => a -> a -> a
powers 0 _ = 0
powers _ 0 = 1
powers n i
       | even i = (powers n (i `div` 2)) * (powers n (i `div` 2))
       | odd i  = n * powers n (i-1)

eulerFuncTR :: (Integral a) => a -> a -> a
eulerFuncTR 1001 g = g
eulerFuncTR n g  = eulerFuncTR (n+1) ( (g + (powers n n) ) `mod` 10000000000)

eulerSolution :: Integer
eulerSolution = eulerFuncTR 1 0
