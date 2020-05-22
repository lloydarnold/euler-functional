import Data.List
import Data.Maybe

fibs :: [Integer]
fibs = 1 : 1 : zipWith (+) fibs (tail fibs)

numOfDigits :: (Integral a) => a -> a
numOfDigits 0 = 0
numOfDigits n = 1 + numOfDigits (n `div` 10)

eulerSolution :: Int
eulerSolution = 1 + ( fromMaybe (0) $ elemIndex
                                    (fromMaybe (0) $ find
                                    (\x -> (numOfDigits x) == 1000) fibs) fibs )
