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
        nextRow row = nextRow' $ [S]++row++[S]
        nextRow' (a:b:c:r) | a/=c      = T:nextRow' (b:c:r)
                           | otherwise = S:nextRow' (b:c:r)
        nextRow' _         = []

main = do
    let input = ".^^..^...^..^^.^^^.^^^.^^^^^^.^.^^^^.^^.^^^^^^.^...^......^...^^^..^^^.....^^^^^^^^^....^^...^^^^..^"
    let solver h = (length . filter (==S) .concat . take h . grid . parseLine)
    print $ solver 40 input
    print $ solver 400000 input