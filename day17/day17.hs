{-# LANGUAGE MonadComprehensions #-}

import qualified Data.ByteString.Char8 as BS 
import qualified Crypto.Hash.MD5 as MD5
import qualified Data.ByteString.Base16 as BS16
import qualified Data.Set as Set
import Data.List (maximumBy)

-- Define data types
data Dir     = U | R | D | L deriving (Eq, Show)
type Coord   = (Int, Int)
type Size    = (Int, Int)
type State   = (Coord, [Dir])
type Visited = Set.Set Coord

-- Generate a hash given a password and a list of moves 
hash :: String -> [Dir] -> String 
hash pass moves = hash $ pass ++ concatMap show moves 
    where hash        = BS.unpack . BS16.encode . MD5.hash . BS.pack

-- Find shortest path to the exit given the password
bfsShortestPath :: String -> [[Dir]]
bfsShortestPath pass = bfs [((0,0), [])] Set.empty
    where gen = hash pass 
          bfs [] _ = []
          bfs (c@((x,y), moves):rest) visited | (x==3 && y==3) = [moves] ++ bfs (rest) visited
                                              | otherwise      = bfs (rest++newStates) (Set.insert (x,y) visited)
            where newStates        = filterValid $ map (mutState c) $ next gen moves 
                  filterValid      = filter inside
                  inside ((x,y),_) = x>=0 && x<= 3 && y>=0 && y<=3


-- Return a list of possible moves given already taken ones (check open doors)
next :: ([Dir] -> String) -> [Dir] -> [Dir]
next gen moves = [a | (a,b) <- zip [U,D,L,R] open, b]
    where open = map (`elem` "bcdef") $ take 4 $ gen moves

-- Mutate a state 
mutState :: State -> Dir -> State 
mutState ((x,y), moves) move = ((x+dx, y+dy), moves++[move])
    where dx = case move of L -> -1; R -> 1; _ -> 0;
          dy = case move of U -> -1; D -> 1; _ -> 0;

-- Compare two paths by length
cmpPaths :: [Dir] -> [Dir] -> Ordering
cmpPaths a b = compare (length a) (length b)

main = do 
    print $ concatMap show $ head $ bfsShortestPath "vkjiggvb" 
    print $ length $ maximumBy cmpPaths $ bfsShortestPath "vkjiggvb" 