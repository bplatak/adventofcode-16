import Data.List.Split (splitOn)
import Data.List       (findIndex)
import Data.Maybe      (fromJust)

-- Define data types 
type Disc = (Int, Int, Int) -- (id, #pos, start pos)

-- Parse a single line of the input 
parseLine :: String -> Disc 
parseLine line = (read (drop 1 i),read n,read (init s)) 
    where (_:i:_:n:_:_:_:_:_:_:_:s:[]) = splitOn " " line 

-- Check if a dics is open if ball dropped at a specific time 
isOpen :: Disc -> Int -> Bool 
isOpen (i,n,s) t = (s+t+i) `mod` n == 0

-- Solve part 1 of the problem 
solve :: [Disc] -> Int 
solve discs = fromJust $ findIndex id ts
    where funcs = map isOpen discs
          ts    = map (\t -> all id $ map ($ t) funcs) [0..]

main = do
    input <- (map parseLine . lines) <$> readFile "day15/data/data.in"

    print $ solve input
 