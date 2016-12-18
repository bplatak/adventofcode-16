-- Define data types
data Type = T | S deriving (Eq, Show)

-- Parse a line 
parseLine :: String -> [Type]
parseLine = map toType 
    where toType x = case x of '^' -> T; '.' -> S;

-- Generate the whole grid 
grid :: [Type] -> [[Type]]
grid = iterate nextRow
    where 
        nextRow row =  (map toType' . window) ([S] ++ row ++ [S]) 
        toType' (T:T:S:[]) = T 
        toType' (S:T:T:[]) = T 
        toType' (T:S:S:[]) = T 
        toType' (S:S:T:[]) = T 
        toType' _          = S
        -- toType  x   = case elem x [[T,T,S], [S,T,T], [T,S,S], [S,S,T]] of 
        --     True -> T; False -> S;

-- Get a sliding window 
window (a:b:c:rest) = [a:b:c:[]] ++ window (b:c:rest)
window _            = []

main = do
    let input = ".^^..^...^..^^.^^^.^^^.^^^^^^.^.^^^^.^^.^^^^^^.^...^......^...^^^..^^^.....^^^^^^^^^....^^...^^^^..^"
    let solver h = (length . filter (==S) .concat . take h . grid . parseLine)
    print $ solver 40 input
    print $ solver 400000 input