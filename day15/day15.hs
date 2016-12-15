import Data.List.Split (splitOn)
import Data.List       (findIndex)
import Data.Maybe      (fromJust)

-- Define data types 
type Disc = (Int, Int, Int) -- (id, #pos, start pos)

-- Check if a dics is open if ball dropped at a specific time 
isOpen :: Disc -> Int -> Bool 
isOpen (i,n,s) t = (s+t+i) `mod` n == 0

-- Solve part 1 of the problem 
solve :: [Disc] -> Int 
solve discs = fromJust $ findIndex id ts
    where funcs = map isOpen discs
          ts    = map (\t -> all id $ map ($ t) funcs) [0..]

main = do
    let input1 = [(1,17,5),(2,19,8),(3,7,1),(4,13,7),(5,5,1),(6,3,0)]
    let input2 = input1++[(7,11,0)]

    print $ solve input1
    print $ solve input2