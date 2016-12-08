{-# LANGUAGE ViewPatterns #-}

import Data.List (stripPrefix, transpose)
import Data.List.Split (splitOn)

-- Define types
type Row         = [Bool]
type MaybeRow    = [Maybe Bool]
type Screen      = [Row]
type MaybeScreen = [MaybeRow]
type Mutator     = Screen -> Screen 

-- Parse the command 
parse :: String -> Mutator 
parse (stripPrefix "rect " -> Just args)            = rect(vals!!0, vals!!1) 
    where vals = map read $ splitOn "x" args
parse (stripPrefix "rotate column x=" -> Just args) = transpose . rot (vals!!0, vals!!1) . transpose
    where vals = map read $ splitOn " by " args 
parse (stripPrefix "rotate row y=" -> Just args)    = rot(vals!!0, vals!!1)
    where vals = map read $ splitOn " by " args 

-- Define the RECT mutation
rect :: (Int, Int) -> Mutator
rect (w,h) screen = mergeScreen screen new
    where new = (replicate h (replicate w (Just True) ++ repeat Nothing)) ++ repeat (repeat Nothing)

-- Rotate a row to the right 
rotRow :: Int -> Row -> Row 
rotRow by row = take (length row) $ drop (length row - by) $ cycle row

-- Merge a row with new values 
mergeRow :: Row -> MaybeRow -> Row
mergeRow a b = map mergeCell $ zip a b
    where mergeCell (a, Just b) = b 
          mergeCell (a, _)      = a

-- Merge two screens together 
mergeScreen :: Screen -> MaybeScreen -> Screen
mergeScreen (a:aRest) (b:bRest) = [(mergeRow a b)] ++ (mergeScreen aRest bRest)
mergeScreen [] _ = []

-- Rotate a row (Y) 
rot :: (Int, Int) -> Mutator 
rot (i, by) screen = mergeScreen screen m
    where nothing a = replicate a $ repeat Nothing
          row       = map Just $ rotRow by (screen!!i)
          m         = nothing i ++ [row] ++ nothing (length screen - i)

-- Print a screen
printScreen :: Screen -> String 
printScreen (row:rest) = map (\x -> if x then '#' else ' ') row ++ "\n" ++ printScreen rest
printScreen [] = ""


main = do
    solver <- (foldl (.) id) . reverse . (map parse) . lines <$> readFile "day8/data/data.in"
    let count = (length . filter (==True) . concat . solver)
    let screen = replicate 6 $ replicate 50 False

    print $ count screen
    putStr $ printScreen $ solver screen