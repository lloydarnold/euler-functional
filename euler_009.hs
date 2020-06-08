pythTrips :: [(Integer, Integer, Integer)]
pythTrips = [(a, b, c) | c <- [300..1000], b <- [1..c], a <- [1..b],
              a^2 + b^2 == c^2, a + b + c == 1000]

prodTrip :: (Integer, Integer, Integer) -> Integer
prodTrip (a,b,c) = a * b * c
