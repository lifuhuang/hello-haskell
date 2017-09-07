import Data.List

solveRPN :: (Floating a, Num a, Read a) => String -> a
solveRPN = head . foldl' process [] . words
    where   process (r:l:xs) "+" = (l + r):xs
            process (r:l:xs) "-" = (l - r):xs
            process (r:l:xs) "*" = (l * r):xs
            process (r:l:xs) "/" = (l / r):xs
            process (r:l:xs) "^" = (l ** r):xs
            process (x:xs) "ln" = log x:xs
            process xs "sum" = [sum xs]
            process xs numberString = read numberString:xs
