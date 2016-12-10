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


test1 = "value 5 goes to bot 2"
test2 = "bot 2 gives low to bot 1 and high to bot 0"
test3 = "bot 1 gives low to output 1 and high to bot 0"
test4 = "bot 0 gives low to output 2 and high to output 0"
test5 = "value 2 goes to bot 2"
test6 = "value 3 goes to bot 1"
test = map words [test1, test2, test3, test4, test5, test6]


bots = readBots test 
assi = readAssignments test 

botAssi = filter (\(_,n) -> byBotType n) assi











    -- parse the input file
    -- input <- map (parse.words) . lines <$> readFile "day10/data/test1.in"
    -- let bots = sortBy compareCmpInstr $ filterCompareInstr input 
    -- let vals = filterValueInstr input

    -- print $ map mapValueInstr $ groupValues vals



-- -- Types 
-- data NodeType   = Bot | Output deriving (Eq, Show) 
-- type Node       = (NodeType, Int)
-- type ValueInstr = (Int, Node)
-- type CmpInstr   = (Int, Node, Node)
-- data Instr      = C CmpInstr | V ValueInstr deriving (Eq, Show)
-- type Comparison = (Node, Node)
-- type Assignment = (Int, Int, Int)

-- -- Parse node type (bot | output) to NodeType
-- parseNodeType :: String -> NodeType
-- parseNodeType "bot"    = Bot
-- parseNodeType "output" = Output

-- -- Parse an instruction into Instr type 
-- parse :: [String] -> Instr 
-- parse ("value":val:_:_:outType:outNum:[]) = 
--     V (read val, (parseNodeType outType, read outNum))
-- parse ("bot":botNum:_:_:_:lOutType:lOutNum:_:_:_:hOutType:hOutNum:[]) = 
--     C (read botNum, (parseNodeType lOutType, read lOutNum), (parseNodeType hOutType, read hOutNum))

-- -- Filter compare instructions
-- filterCompareInstr :: [Instr] -> [CmpInstr]
-- filterCompareInstr (C instr:rest) =  [instr] ++ filterCompareInstr rest 
-- filterCompareInstr (_:rest) = filterCompareInstr rest 
-- filterCompareInstr [] = []

-- -- Filter value instructions 
-- filterValueInstr :: [Instr] -> [ValueInstr]
-- filterValueInstr (V instr:rest) = [instr] ++ filterValueInstr rest 
-- filterValueInstr (_:rest) = filterValueInstr rest 
-- filterValueInstr [] = []

-- -- Compare CmpInstructions (order by bot num)
-- compareCmpInstr :: CmpInstr -> CmpInstr -> Ordering 
-- compareCmpInstr (a, _, _) (b, _, _) = compare a b

-- eqValueInstr :: ValueInstr -> ValueInstr -> Bool
-- eqValueInstr (_, (_,a)) (_, (_,b)) = a==b

-- -- Generate all consequent value instructions
-- groupValues = groupBy eqValueInstr

-- mapValueInstr :: [ValueInstr] -> Assignment
-- mapValueInstr ((a,(_,i)):(b,_):[]) = (i, a, b)


-- lowNode :: CmpInstr -> Node 
-- lowNode (_, n, _) = n 

-- highNode :: CmpInstr -> Node 
-- highNode (_, _, n) = n

-- unpackAssignment :: [CmpInstr] -> Assignment -> [ValueInstr]
-- unpackAssignment bots (i, a, b) = [(max a b, high (bots!!i)), (min a b, low (bots!!i))]
--     where high (_, _, n) = n 
--           low (_, n, _)  = n



-- -- Get all nodes present in the problem 
-- getNodes :: [Instr] -> [Node]
-- getNodes (ValueInstr (_,n1):rest)     = nub $ [n1] ++ getNodes rest 
-- getNodes (CmpInstr   (_,n1, n2):rest) = nub $ [n1,n2] ++ getNodes rest 
-- getNodes []                           = []

-- -- Create node states list 
-- mkNodeStates :: [Node] -> [NodeState]
-- mkNodeStates nodes = zip nodes $ repeat (0, [])

-- -- findBot :: Comparison -> [Instr] -> Int 
-- -- findBot (low, high) (instr:rest) = 

-- execInstr :: Instr -> [NodeState] -> [NodeState]
-- execInstr (ValueInstr (val, node)) nodes 
