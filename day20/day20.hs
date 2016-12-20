import Data.List.Split (splitOn)
import Data.List       (sortBy)
import Data.Ord        (comparing)
import Debug.Trace 

-- Define types
type Range = (Int, Int)

-- Parse an input line (range)
parseLine :: String -> Range 
parseLine str = (a,b) where [a,b] = (map read . splitOn "-") str 

-- Find lowest allowed IP
lowestPermitted :: [Range] -> Int 
lowestPermitted = ((+ 1) . snd . head . mergeRanges . sortBy (comparing fst))

-- Count number of permitted addresses
countPermitted :: Int -> [Range] -> Int 
countPermitted m = (count . mergeRanges . sortBy (comparing fst))
    where count ((_, ah):b@(bl, _):r) = bl - ah - 1 + count (b:r)
          count ((_, ah):[])          = m - ah

-- Merge the ranges
mergeRanges :: [Range] -> [Range]
mergeRanges (a@(al, ah):b@(bl, bh):r) | bl<=ah+1  = mergeRanges ((al, max ah bh):r)
                                      | otherwise = a:(mergeRanges (b:r))
mergeRanges (a:[])                    = [a]

main = do 
    input <- map parseLine . lines <$> readFile "day20/data/data.in"
    print $ lowestPermitted input 
    print $ countPermitted 4294967295 input 