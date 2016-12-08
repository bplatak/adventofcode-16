import Data.List
import Data.List.Split

-- Convert a line to an input
lineToInput line = map read (words line)

-- Is triangle
isTriangle input = (sum input - max) > max
    where max = maximum input

main = do
    fileData <- readFile "day3/data/data.in"
    print $ length $ filter isTriangle $ chunksOf 3 $ concat $ transpose $ map lineToInput (lines fileData)