{-# LANGUAGE ViewPatterns #-} 
import qualified Data.Map as M
import Text.Read (readMaybe)
import Data.List (sort)
import Data.Maybe (fromJust)

-- Define types 
type Regs  = M.Map Reg Int 
type State = ([Instr], Int, Regs) --instrs, pc, regs 
type Reg   = Char 
data Instr = Cpy Expr Reg  | Inc Reg  | Dec Reg | 
             Jnz Expr Expr | Tgl Expr | Nop     |
             Mul Reg Reg   | AddM Reg Reg | Sub Expr Reg deriving Show 
data Expr  = L Int | R Reg                     deriving Show  

-- Parse an input line 
parseLine :: String -> Instr 
parseLine (words -> cmd) = case cmd of 
    ("cpy":x:(y:[]):[])  -> Cpy (e x) y
    ("inc":(x:[]):[])    -> Inc x
    ("dec":(x:[]):[])    -> Dec x
    ("jnz":x:y:[])       -> Jnz (e x) (e y)
    ("tgl":x:[])         -> Tgl (e x)
    ("mul":(x:[]):(y:[]):[])-> Mul x y
    ("addm":(x:[]):(y:[]):[])  -> AddM x y
    ("nop":[])           -> Nop
    where e (readMaybe -> Just x) = L x 
          e x                     = R (x!!0)

-- Execute an instruction
execInstr :: State -> Instr -> State 
execInstr (instrs, pc, regs) instr = case instr of 
    Nop     -> (instrs, pc+1, regs)
    Cpy x y -> (instrs, pc+1, M.insert y (e x) regs)
    Inc x   -> (instrs, pc+1, M.insert x (e (R x) + 1) regs)
    Dec x   -> (instrs, pc+1, M.insert x (e (R x) - 1) regs)
    AddM x y-> (instrs, pc+1, nregs)
        where nregs = M.insert x 0 . M.insert y (e (R x) + e (R y)) $ regs
    Jnz x y -> case (e x) of 
        0         -> (instrs, pc+1, regs)
        otherwise -> case (e y) of 
            0         -> error "Infinite loop detected"
            otherwise -> (instrs, pc+(e y), regs)
    Mul x y -> (instrs, pc+1, M.insert y ((e (R x)) * (e (R y))) regs)
    Tgl x     -> (tgl (pc + (e x)), pc+1, regs)
    where 
        e (R x) = case M.lookup x regs of Just y -> y; Nothing -> 0
        e (L x) = x 
        tgl x | x>=length instrs = instrs 
              | otherwise        = (take x instrs) ++ [ninstr (instrs!!x)] ++ (drop (x+1) instrs)
            where 
                ninstr (Inc x)       = Dec x
                ninstr (Dec x)       = Inc x 
                ninstr (Tgl (R x))   = Inc x 
                ninstr (Jnz x (R y)) = Cpy x y 
                ninstr (Cpy x y)     = Jnz x (R y) 
                ninstr _             = Nop

-- Execute the whole program 
execProg :: [Instr] -> [(Reg, Int)] -> Regs 
execProg instrs initial = exec (instrs, 0, M.fromList initial) 0
    where 
        exec s@(instrs, pc, regs) c | pc>=length instrs = regs 
                                    | otherwise         = exec t (c+1)
            where t@(i,p,r) = execInstr s (instrs!!pc)

main = do 
    input <- map parseLine . filter (/="") . lines <$> readFile "day23/data/data.in"

    print $ M.lookup 'a' $ execProg (replaceLoops input $ findLoops input) [('a', 7)]
    print $ M.lookup 'a' $ execProg (replaceLoops input $ findLoops input) [('a', 12)]

-- Find loops in the code (only with constant jump distance) 
type Loop = (Int, Int, Reg, [Instr]) -- from, to, register, instrs 

findLoops :: [Instr] -> [Loop]
findLoops instrs = map toLoop . filter (isConstJump.snd) . zip [0..] $ instrs
    where 
        isConstJump (Jnz (R _) (L y)) = True 
        isConstJump _                 = False
        toLoop (pc, Jnz (R x) (L y))  = (pc,pc+y, x, take (b-a) . drop a $ instrs)
            where (a:b:[]) = sort [pc, pc+y]

optimiseLoop :: Loop -> [Instr]
optimiseLoop (f,t,reg,instrs) | optimisable inInstrs =  new ++ replicate (1+f-t - length new) Nop
                              | otherwise            = instrs  
    where
        optimisable (Cpy _ _:rest) = True && optimisable rest 
        optimisable (Inc _:rest)   = True && optimisable rest 
        optimisable (AddM _ _:rest)= True && optimisable rest 
        optimisable (Nop:rest)     = True && optimisable rest 
        optimisable (_:rest)       = False 
        optimisable []             = True 
        inInstrs = filter notTarget instrs 
            where notTarget (Dec x)   = x/=reg 
                  notTarget (Inc x)   = x/=reg 
                  notTarget (Cpy _ x) = x/=reg 
                  notTarget (AddM _ x)= x/=reg
                  notTarget (Jnz _ _) = False 
                  notTarget Nop       = False
        new = s inInstrs
        s ((Inc x):rest)                = [AddM reg x] ++ s rest 
        s (Cpy (R x) y:AddM a b:rest)   = [Mul x reg, AddM reg b] ++ s rest
        s (_:r)                         = Nop:s r
        s []                            = []


replaceLoop :: [Instr] -> Loop -> [Instr]
replaceLoop instrs l@(f,t,_,_) = concat [take t instrs, optimiseLoop l, drop (f+1) instrs]

replaceLoops :: [Instr] -> [Loop] -> [Instr]
replaceLoops instrs (l:_) | length next > 2 = replaceLoops new (findLoops new) -- hacky check for length
                          | otherwise       = new 
    where new  = replaceLoop instrs l
          next = findLoops new  
replaceLoops instrs _        = instrs