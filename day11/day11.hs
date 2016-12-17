import Data.List ((\\), subsequences, sort)
import qualified Data.Set as Set 

-- Define data types 
data Type  = Li | H | Pm | Co | Cm | Ru | Pu | Cu deriving (Eq, Show, Ord)
data Item  = Chip Type | Gen Type                 deriving (Eq, Show, Ord)
data Dir   = U | D                                deriving (Eq, Show)
type State = (Int, [[Item]], Int) -- (floor, items, moves so far)
type Move  = (Dir, [Item])
type Visited = Set.Set [[Item]]

-- Check if a combination of items is legal 
isLegal :: [Item] -> Bool 
isLegal items = (chips \\ gens) == [] || gens == []
  where gens  = [ x | (Gen x) <- items]
        chips = [ x | (Chip x) <- items]

-- Generate possible elevator loads from items list (length 0,1 or 2)
mkItems :: [Item] -> [[Item]]
mkItems = filter len . subsequences
  where len x = elem (length x) [0,1,2]

-- Generate all next movies 
nextMoves :: State -> [Move]
nextMoves s@(f, its, steps) = moves
  where items = filter isLegal $ mkItems (its!!f)  
        moves = [(x,y) | x <- dirs, y <- items]
        dirs | f==0            = [U]
             | f==length its-1 = [D]
             | otherwise       = [U,D]

-- Generate all following states 
nextStates :: State -> [State]
nextStates s@(f, its, steps) = states 
  where 
    states   =  filter legal $ map toState $ nextMoves s
    legal (_,x,_) = all isLegal x 
    toState (d, i) = (nf, items, ns)::State
      where
        nf     = case d of U -> f+1; D -> f-1;
        remits = take f its ++ [(its!!f)\\i] ++ drop (f+1) its 
        items  = take nf remits ++ [(remits!!nf)++i] ++ drop (nf+1) remits
        ns     = steps+1


-- BFS search until a solutions is found 
bfs :: [State] -> Visited -> State 
bfs (c@(f,i,s):rest) visited | finished  = c
                             | otherwise = bfs (rest ++ newStates) (Set.insert (sorted i) visited)
  where 
    finished  = all (\x -> length x == 0) $ take (length i - 1) i
    sorted    = map sort
    newStates = filter (not.isQueued) $ filter (not.isVisited) $ nextStates c
      where
        isQueued  (_,x,_) = elem (sorted x) $ map (\(_,y,_) -> sorted y) rest 
        isVisited (_,x,_) = Set.member (sorted x) visited


main = do 
  print "aa"

test = (0, [[Chip H, Chip Li], [Gen H], [Gen Li], []], 0)::State

input = (0, [[Gen Pm, Chip Pm], [Gen Co, Gen Cm, Gen Ru, Gen Pu], [Chip Co, Chip Cu, Chip Ru, Chip Pu], []], 0)::State