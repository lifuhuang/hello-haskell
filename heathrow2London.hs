import Data.List

data Section = Section { getA :: Int, getB :: Int , getC :: Int } deriving (Show)
type RoadSystem = [Section]

data Label = A | B | C deriving (Show)
type Path = [(Label, Int)]


heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]  

roadStep :: (Path, Int, Path, Int) -> Section -> (Path, Int, Path, Int)
roadStep (pathA, distA, pathB, distB) (Section a b c) = 
    let straightToA = distA + a
        straightToB = distB + b
        crossToA = distB + b + c
        crossToB = distA + a + c
        newPathToA = if straightToA < crossToA
            then ((A, a):pathA, straightToA)
            else ((C, c):(B, b):pathB, crossToA)
        newPathToB = if straightToB < crossToB
            then ((B, b):pathB, straightToB)
            else ((C, c):(A, a):pathA, crossToB)
    in  (fst newPathToA, snd newPathToA, fst newPathToB, snd newPathToB)

optimalPath :: RoadSystem -> Path
optimalPath roadSystem = 
    let (pathA, distA, pathB, distB) = foldl' roadStep ([], 0, [], 0) roadSystem
        result = if distA < distB 
            then pathA
            else pathB
    in  reverse result

groupsOf :: Int -> [a] -> [[a]]  
groupsOf 0 _ = undefined  
groupsOf _ [] = []  
groupsOf n xs = take n xs : groupsOf n (drop n xs)  

main = do
    contents <- getContents
    let threes = groupsOf 3 $ map read $ lines contents
        roadSystem = map (\[a, b, c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ map (show . fst) path
        pathDist = sum $ map snd path
    putStrLn $ "The best path to take is: " ++ pathString  
    putStrLn $ "The price is: " ++ show pathDist