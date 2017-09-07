import Data.List

solveRPN :: (Fractional a, Num a, Read a) => String -> a
solveRPN = head . foldl' process [] . words
    where   process (r:l:xs) "+" = (l + r):xs
            process (r:l:xs) "-" = (l - r):xs
            process (r:l:xs) "*" = (l * r):xs
            process (r:l:xs) "/" = (l / r):xs
            process xs numberString = read numberString:xs
