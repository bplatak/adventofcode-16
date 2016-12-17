import Data.List ((\\), subsequences, sort)
import qualified Data.Set as Set 
import Debug.Trace 

-- Define data types 
data Type  = H | Li | Pm | Co | Cu | Ru | Pu | El | Di deriving (Eq, Show, Ord)
data Item  = Chip Type | Gen Type                      deriving (Eq, Show, Ord)
data Dir   = U | D                                     deriving (Eq, Show)
type State = (Int, [[Item]], [Move]) -- (floor, items, moves so far)
type Move  = (Dir, [Item])
type Visited = Set.Set (Int, [[Item]])

-- Check if a combination of items is legal 
isLegal :: [Item] -> Bool 
isLegal items = (chips \\ gens) == [] || gens == []
  where gens  = [ x | (Gen x) <- items]
        chips = [ x | (Chip x) <- items]

-- Generate possible elevator loads from items list (length 0,1 or 2)
mkItems :: [Item] -> [[Item]]
mkItems = filter len . subsequences
  where len x = elem (length x) [1,2]

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
nextStates s@(f, its, moves) = states 
  where 
    states   =  filter legal $ map (mutState s) $ nextMoves s
    legal (_,x,_) = all isLegal x 


-- Mutate a state given a move 
mutState :: State -> Move -> State 
mutState s@(f, its, moves) m@(d, i) = (nf, items, ns)::State
  where
    nf     = case d of U -> f+1; D -> f-1;
    remits = take f its ++ [(its!!f)\\i] ++ drop (f+1) its 
    items  = take nf remits ++ [(remits!!nf)++i] ++ drop (nf+1) remits
    ns     = moves++[m]

-- BFS search until a solutions is found 
bfs :: [State] -> Visited -> State 
bfs [] visited =  error "No solution!"
bfs (c@(f,i,s):rest) visited | finished  = c
                             | otherwise = bfs (rest ++ newStates) (Set.insert (f, sorted i) visited)
  where 
    finished  = all (\x -> length x == 0) $ take (length i - 1) i
    sorted    = map sort
    ta        = traceShow (f, sorted i)
    newStates = filter (not.isQueued) $ filter (not.isVisited) $ nextStates c
      where
        isQueued  (_,x,_) = elem (sorted x) $ map (\(_,y,_) -> sorted y) rest 
        isVisited (a,x,_) = Set.member (a, sorted x) visited


main = do 
  -- Solve part 1
  let part1 = (0, [[Gen Pm, Chip Pm], [Gen Co, Gen Cu, Gen Ru, Gen Pu], [Chip Co, Chip Cu, Chip Ru, Chip Pu], []], [])::State
  let (_, _, s) = bfs [part1] Set.empty 
  print $ length s 
  print $ s 

  -- Solve part 2
  let part2 = (0, [[Gen El, Chip El, Gen Di, Chip Di, Gen Pm, Chip Pm], [Gen Co, Gen Cu, Gen Ru, Gen Pu], [Chip Co, Chip Cu, Chip Ru, Chip Pu], []], [])::State  
  let (_, _, s) = bfs [part2] Set.empty 
  print $ length s 
  print $ s 