import Data.List.Split
import Data.Char

data Bearing    = N | E | S | W deriving (Eq, Show, Enum, Bounded)
data Direction  = R | L | None deriving (Eq, Show, Enum, Bounded)
type Coordinate = (Int, Int)
type Move       = (Direction, Int)
type NormalMove = (Bearing, Int)
type State      = (Bearing, Coordinate)
type History    = [Coordinate]

-- Decompose a command
decomposeMove :: [Char] -> Move
decomposeMove (dir:n) = ((charToDir dir), (read n :: Int))

-- Convert a character to direction
charToDir :: Char -> Direction
charToDir char = case char of
    'R' -> R
    'L' -> L

-- Direction to turn modifier
dirToInt :: Direction -> Int
dirToInt dir = case dir of
    R -> 1
    L -> (-1)
    otherwise -> 0

-- Move the enum around
turn :: (Enum a, Bounded a) => a -> Int -> a
turn enum amount = do
    let count = fromEnum (maxBound `asTypeOf` enum) + 1
    toEnum (((fromEnum enum) + amount) `mod` count)

-- Mutate state
step :: State -> Move -> State
step (bearing, (x,y)) (dir, amount) = do
    let newBearing = turn bearing (dirToInt dir)
    case newBearing of
        N -> (newBearing, (x, y+amount))
        E -> (newBearing, (x+amount, y))
        S -> (newBearing, (x, y-amount))
        W -> (newBearing, (x-amount, y))

-- Solve the first one
solve :: State -> [Move] -> Int
solve state (move:rest) = solve (step state move) rest
solve (_, (x,y)) [] = abs(x) + abs(y)

-- Solve the second one
solve2 :: [Bearing] -> History -> Int
solve2 (move:rest) history
        | elem newCoord history = abs(fst newCoord) + abs(snd newCoord)
        | otherwise = solve2 rest (history ++ [newCoord])
    where
        newCoord = snd (step (move, last history) (None, 1))


-- Normalize moves
normalizeMove :: Bearing -> Move -> [Bearing]
normalizeMove bearing (dir, amount) = do
    let newBearing = turn bearing (dirToInt dir)
    replicate amount newBearing

normalizeMoves :: Bearing -> [Move] -> [Bearing]
normalizeMoves bearing [] = []
normalizeMoves bearing (move:rest) = do
    let newMoves = normalizeMove bearing move
    newMoves ++ normalizeMoves (last newMoves) rest

main = do
    input <- readFile "day1/data/data.in"
    let inputData = map (decomposeMove . unwords . words) (splitOn "," input)
    print (solve (N, (0,0)) inputData)
    print (solve2 (normalizeMoves N inputData) [(0,0)])