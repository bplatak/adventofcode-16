import Data.Bits (popCount)
import qualified Data.Map as Map 
import Debug.Trace 

-- Define all types 
type Index = (Int, Int)                         -- (x,y)
type Edges = [Index]                            -- list of connecting edges
type Prob  = (Int, Int, Int)                    -- (key, width, height)
type State = (Index, Int, Map.Map Index Bool)   -- (current, steps, visited)

-- Determine if a specific cell index represents a wall
isWall :: Int -> Index -> Bool 
isWall key (x,y) = odd $ popCount num 
    where num = x*x+3*x+2*x*y+y+y*y+key


-- Generate all edges of a specific cell
mkEdges :: Int -> Index -> Edges 
mkEdges key (x,y) = filter (not.isWall key) adjs
    where adjs = wB [(x, y-1), (x+1, y), (x,y+1), (x-1,y)]
          wB   = filter (\(a,b) -> not (a<0 || b<0))

-- Find the shortest path length 
bfsPathLength :: Int -> Index -> [State] -> Int 
bfsPathLength key goal t@((curr, steps, visited):rest)  | goal==curr = steps
                                                        | otherwise  = bfsPathLength key goal newStates
    where newStates         = (filter (not.isQueued) $ filter (not.isVisited) $ map mkStates $ mkEdges key curr) ++ rest 
          mkStates s        = (s, steps+1, Map.insert curr True visited)
          isVisited (s,_,_) = Map.member s visited
          isQueued  (s,_,_) = elem s $ map (\(x,_,_) -> x) t

-- Calculate the number of reachable nodes from a starting position
bfsReachableLocations :: Int -> [(Int, Index)] -> [Index] -> Int 
bfsReachableLocations key a@((lim,curr):rest) visited | lim <  0  = 0 
                                                      | otherwise = 1 + bfsReachableLocations key newNodes (visited++[curr])
    where neighbours = filter (not.isQueued) $ filter (not.(\x -> elem x visited)) $ mkEdges key curr
          isQueued n = elem n $ map (\(_,x) -> x) a
          newNodes   = rest ++ map (\x -> (lim-1, x)) neighbours

-- Print the maze
-- printMaze :: Int -> Int -> Int -> [[Bool]] 
printMaze key w h = map genRow [0..h-1] 
    where genRow y = concat $ map (\x -> if isWall key (x,y) then "\t#" else "\t.") [0..w-1]

main = do 
    print $ bfsPathLength           1362 (31,39) [((1,1), 0, Map.empty)]
    print $ bfsReachableLocations   1362 [(50, (1,1))] [] 

