import Data.List.Split
import Data.Char

type Position = Int
data Move     = U | D | L | R deriving (Eq, Show, Enum)
type MovesMap = [[Int]]

movesMap1 = [
    [1,4,1,2], [2,5,1,3], [3,6,2,3],
    [1,7,4,5], [2,8,4,6], [3,9,5,6],
    [4,7,7,8], [8,8,7,9], [6,9,8,9]]
movesMap2 = [
                                   [1, 3, 1, 1],
                  [2, 6, 2, 3],    [1, 7, 2, 4],    [4, 8, 3, 4],
    [5, 5, 5, 6], [2, 10, 5, 7],   [3, 11, 6, 8],   [4, 12, 7, 9], [9, 9, 8, 9],
                  [6, 10, 10, 11], [7, 13, 10, 12], [8, 12, 11, 12],
                                   [11, 13, 13, 13]]
charsMap = "0123456789ABCDEF"

-- Char to move
charToMove :: Char -> Move
charToMove char = case char of
    'U' -> U; 'D' -> D; 'L' -> L; 'R' -> R

-- Perform a single step
step ::  Position -> Move -> MovesMap -> Position
step position move movesMap = (movesMap !! (position-1)) !! (fromEnum move)

-- Decode a single digit
decode :: Position -> [Move] -> MovesMap -> Int
decode position (move:rest) movesMap = decode (step position move movesMap) rest movesMap
decode position [] _                 = position

-- Solve the problem
solve :: Position -> [[Move]] -> [Int] -> MovesMap -> [Int]
solve position (moves:rest) result movesMap = do
    let digit = decode position moves movesMap
    solve digit rest (result ++ [digit]) movesMap
solve position [] result _ = result

main = do
    rawInput <- readFile "day2/data/data.in"
    let input = map (map charToMove) (lines rawInput)
    let func = solve 5 input []
    print (map (\x -> charsMap !! x) (func movesMap1))
    print (map (\x -> charsMap !! x) (func movesMap2))