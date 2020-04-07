primes = sieve [2..]
sieve (p:ps) = p : sieve [ x | x <- ps, mod x p /= 0 ]

main :: IO()
main = print(sum (takeWhile (<2000000) primes))
