{-- Fundamentally, this is a combinatorics problem (and almost identical to
    one I was asked in my Ox interview how wild). I did much of the intellectual
    legwork on paper before coming here.  --}

factorial :: (Integral a) => a -> a
factorial 0 = 1
factorial n = n * factorial (n-1)

-- this is all wrong (oops !)
driver :: (Integral a) => a -> a -> a -> a
driver n k s
      | k == n - 1  = s
      | otherwise   = driver n (k+1) s + g
      where g = factorial (n+k-1) `div` ( factorial (n-k) * factorial k)

howManySums :: (Integral a) => a -> a
howManySums n = driver n 2 0
