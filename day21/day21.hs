{-# LANGUAGE ViewPatterns #-}
import Data.List    (stripPrefix, sort, (\\), findIndex, find, reverse)
import Data.Maybe   (fromJust)

-- Define the types
type Mutator = String -> String 
data Instr   = SwapPos (Int, Int)   |
               SwapLet (Char, Char) |
               RotInd  (Char)        |
               RotDir  (Dir, Int)   |
               Rev     (Int, Int)   |
               Mov     (Int, Int)       deriving (Show)             
data Dir     = L | R                    deriving (Show)

-- -- Parse an input line 
parseLine :: String -> Instr 
parseLine (words -> w) = case w of 
    ["swap", "position", x, _, _, y]    -> SwapPos (read x, read y)
    ["swap", "letter", x, _, _, y]      -> SwapLet (x!!0, y!!0)
    ["rotate", "based", _, _, _, _, x]  -> RotInd  (x!!0)
    ["rotate", dir, x, _]               -> RotDir  (p dir, read x)
        where p d = case d of "left" -> L; "right" -> R;
    ["reverse", _, x, _, y]             -> Rev     (read x, read y)
    ["move", _, x, _, _, y]             -> Mov     (read x, read y)

-- Swap position of two letters 
swapPos :: (Int, Int) -> Mutator
swapPos (a,b) str = map swap . zip [0..] $ str 
    where swap (i,l) | i==a = str!!b | i==b = str!!a | otherwise = l

-- Swap letters by value 
swapLet :: (Char, Char) -> Mutator
swapLet (a,b) = map swap
    where swap x | x==a = b | x==b = a | otherwise = x

-- Rotate indirect (based on position)
rotIndirect :: Char -> Mutator 
rotIndirect a str = rotDirect (R, if by<4 then by+1 else by+2) str 
    where by = fromJust $ findIndex (==a) str 

-- Rotate direct 
rotDirect :: (Dir, Int) -> Mutator 
rotDirect (d,a) str = take (length str) . drop by $ cycle str  
    where by  = case d of L -> a; R -> (length str)*2 - a;

-- Reverse part of the string 
rev :: (Int, Int) -> Mutator 
rev (a,b) str = take a str ++ (reverse . take (b-a+1) . drop a) str ++ drop (b+1) str

-- Move a character from one position to another 
mov :: (Int, Int) -> Mutator 
mov (x, y) str = map (case x<y of True -> m; False -> m') $ zip [0..] str 
    where m (p, l) | p==y = str!!x | p>=x && p<y  = str!!(p+1) | otherwise = l
          m'(p, l) | p==y = str!!x | p>=y && p<=x = str!!(p-1) | otherwise = l

-- Scramle the string given the instructions 
scrambler :: [Instr] -> Mutator
scrambler = (foldl (.) id . map toMut . reverse)
    where toMut (SwapPos x) = swapPos x
          toMut (SwapLet x) = swapLet x
          toMut (RotInd  x) = rotIndirect x 
          toMut (RotDir  x) = rotDirect x 
          toMut (Rev     x) = rev x 
          toMut (Mov     x) = mov x 

-- Unscramble the string 
unscrambler :: [Instr] -> Mutator 
unscrambler = (foldl (.) id . map toMut)
    where toMut (SwapPos (a,b)) = swapPos (b,a)
          toMut (SwapLet (a,b)) = swapLet (b,a)
          toMut (RotInd  x)     = unRotIndirect x
          toMut (RotDir  (d,a)) = case d of L -> rotDirect(R,a); R -> rotDirect(L,a);
          toMut (Rev     x)     = rev x 
          toMut (Mov     (a,b)) = mov (b,a)

-- Unapply indirect rotation
unRotIndirect :: Char -> Mutator 
unRotIndirect c str = rotDirect (L, by) str 
    where by = fromJust $ findIndex (==str) $ map mapper[0..length str+10]
          mapper x = rotIndirect c $ rotDirect (L, x) str 

main = do
    input <- (map parseLine . lines) <$> readFile "day21/data/data.in"
    print $ scrambler input "abcdefgh"
    print $ unscrambler input "fbgdceah"