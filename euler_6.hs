sumSquares :: Int -> Int
sumSquares n = foldl (+) 0 [a^2 | a <- [1..n]]

squareSum :: Int -> Int
squareSum n = (foldl (+) 0 [a | a <- [1..n]])^2

diffNums :: Int -> Int -> Int
diffNums x y = abs(x - y)

main :: IO ()
main = print( diffNums (sumSquares 100) (squareSum 100) )
