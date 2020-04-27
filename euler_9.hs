pythagTrips :: (Integral a) => [(a, a, a)]
pythagTrips = [(a, b, c) | c <- [1..], b <- [1..c], a <- [1..b], a*a + b*b == c*c]

specPythagTrips :: (Integral a) => [(a, a, a)]
specPythagTrips = [(a, b, c) | c <- [1..1000], b <- [1..c], a <- [1..b], a*a + b*b == c*c, a + b + c == 1000]

-- euler solution is product of a b and c in specPythagTrips
