import Data.List (sortBy, groupBy)
import Data.Maybe (fromJust, isJust)
import Debug.Trace 


data Node       = BotType Int | OutputType Int deriving (Show, Eq)
type Bot        = (Int, Node, Node, Vals) -- bot id, low output, high output 
type Assignment = (Int, Node) -- value, node 
type Vals       = [Int]
type Comparison = (Int, Int, Int) -- id, low, high

-- Parse a bot definition
parseBot :: [String] -> Maybe Bot 
parseBot ("bot":botNum:_:_:_:lOutType:lOutNum:_:_:_:hOutType:hOutNum:[]) = 
        Just (read botNum, (t lOutType) (read lOutNum), (t hOutType) (read hOutNum), [])
    where t x= case x of "bot" -> BotType; "output" -> OutputType
parseBot _ = Nothing 

-- Parse a value assignment 
parseAssignment :: [String] -> Maybe Assignment
parseAssignment ("value":val:_:_:outType:outNum:[]) = 
        Just (read val, (t outType) (read outNum))
    where t x= case x of "bot" -> BotType; "output" -> OutputType
parseAssignment _ = Nothing

-- Read all bots and sort 
readBots :: [[String]] -> [Bot]
readBots lines = sortBy sort $ map fromJust $ filter isJust $ map parseBot lines 
    where sort (a,_,_,_) (b,_,_,_) = compare a b

-- Read assignments
readAssignments :: [[String]] -> [Assignment]
readAssignments lines = map fromJust $ filter isJust $ map parseAssignment lines 

-- Filter by bot type 
byBotType :: Node -> Bool 
byBotType (BotType _) = True 
byBotType _ = False 

-- Filter output type 
byOutputType :: Node -> Bool 
byOutputType (OutputType _) = True 
byOutputType _ = False

-- Check if nodes in two assignments are equal
eqNode :: Assignment -> Assignment -> Bool 
eqNode (_,a) (_,b) = a==b

-- Make assignments
mkAssignments :: Bot -> [Assignment]
mkAssignments (i, lowOut, highOut, (a:b:[])) = [lowAssi, highAssi]
    where lowAssi  = (min a b, lowOut)
          highAssi = (max a b, highOut) 

-- mutateBots :: [Bot] -> Assignment -> [Bot]
-- mutateBots bots@((i, l, h, vals):_) (val, BotType bid) | i == bi =  

comparisons :: [Bot] -> [Assignment] -> [Comparison]
comparisons bots ass@((val, BotType bid):rest) = newComps ++ (comparisons newBots (rest++newAssi))
    where newComps = map (\(i,l,h,v) -> (i,min (v!!0) (v!!1), max (v!!1) (v!!0))) $ filter (\bot@(i,l,h,v) ->  length v == 2) newBots'
          newBots = map (\(i,l,h,v) -> (i,l,h,if length v == 2 then [] else v)) newBots'
          newBots' = map (\((i,l,h,v),b) -> (i,l,h,v++b)) $ zip bots newBots''
          newBots'' = map (\(i) -> if i==bid then [val] else []) [0..(length bots)-1]
          newAssi' = []
          newAssi = concat $ map mkAssignments $ filter (\bot@(i,l,h,v) ->  length v == 2) newBots'
          t = show newBots ++ "      " ++ show (rest++newAssi) 
comparisons bots (_:rest) = comparisons bots rest
comparisons _ [] = []

-- Main entry point 
main = do 
    input <- (map words . lines) <$> readFile "day10/data/data.in"
    let solver a b = filter (\(_,a',b') -> a==a' && b==b')
    print $ solver 17 61 $ comparisons (readBots input) (readAssignments input)