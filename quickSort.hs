quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort (x : xs) =
    let smallerSorted = quickSort $ filter (<= x) xs
        largerSorted = quickSort $ filter (> x) xs
    in  smallerSorted ++ [x] ++ largerSorted