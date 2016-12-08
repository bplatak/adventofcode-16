import Data.List.Split
import Data.List

-- Parse a line of input by splitting it intwo [[outside_bracket], [inside_btacket]]
parse :: String -> [[String]]
parse = (transpose.(chunksOf 2).concat.(map (splitOn "]")).(splitOn "["))

-- Check if chunk is ABBA
isAbba (a:b:c:d:rest) | a==d && b==c && a/=b = True
                      | otherwise            = isAbba (b:c:d:rest)
isAbba _              = False

-- Solve the problem
solve :: [[String]] -> Bool
solve (outside:inside:[]) = (any isAbba outside) && not (any isAbba inside)

-- Get all XYX parts
getXYX :: String -> [String]
getXYX (a:b:c:rest) | a==c && a/=b = [(a:b:c:[])] ++ getXYX (b:c:rest)
                    | otherwise    = getXYX (b:c:rest)
getXYX _            = []

-- Solve the second part of the problem
solve2 :: [[String]] -> Bool
solve2 (outside:inside:[]) = not . null $ intersect (m outside) (((map t).m) inside)
    where m = concat.(filter (not.null)).(map getXYX)
          t (a:b:c:[]) = (b:a:b:[])

main = do
    input <- readFile "day7/data/data.in"

    print $ (length . (filter solve) . (map parse) . lines) input
    print $ (length . (filter solve2) . (map parse) . lines) input