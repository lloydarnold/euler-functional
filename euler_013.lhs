Work out the first ten digits of the sum of the following one-hundred 50-digit
numbers. This isn't a hard problem but big step for me because first time I'm
reading a file into Haskell ! :)

> main :: IO ()
> main = do { contents <- readFile "./src/euler_013_src.txt";
>             putStrLn $ composition contents }

Okay so after reading it in, it is a string, containing \n chars. In hask, a
string is equivalent to [Char]. I need to split the [Char] into [ [Char] ],
and we know that each number is 50 chars long.

Tail + reverse is because haskell is a bitch / splitString left "" as the final
term.

N.B. for technical reasons (me being a chump) we just calculated the whole sum
and manually took the first ten.

> composition = show . sum . numbify . tail . reverse . splitString

> splitString :: [Char] -> [[Char]]
> splitString []   = [[]]
> splitString (xs) = take 50 xs : splitString ( drop 51 xs )

> numbify :: [[Char]] -> [Integer]
> numbify []     = []
> numbify (x:xs) = read x : numbify xs
