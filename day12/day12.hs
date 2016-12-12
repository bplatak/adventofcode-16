import Text.Read
import qualified Data.Map as Map 
import Data.Maybe 
import Debug.Trace

type Reg   = [Char]
data Ref   = Reg Reg | Val Int deriving (Eq, Show)
data Instr = Cpy Ref Reg | Inc Reg | Dec Reg | Jnz Ref Int deriving (Eq, Show)
type State = (Map.Map Reg Int, Int) --regs, pc

-- Parse an input line 
parse :: String -> Instr 
parse line = let tokens = words line in case tokens of 
    ("cpy":arg1:reg:[]) -> Cpy (parseRef arg1) reg
    ("inc":reg:[])      -> Inc reg
    ("dec":reg:[])      -> Dec reg 
    ("jnz":val:by:[])   -> Jnz (parseRef val) (read by)

-- Parse reference (either literal or register)
parseRef :: [Char] -> Ref
parseRef arg = case readMaybe arg :: Maybe Int of 
    Just val -> Val val 
    _        -> Reg arg 

-- Execute the program 
execProg :: State -> [Instr] -> State 
execProg s@(regs,pc) instrs | length instrs <= pc = s 
                            | otherwise           = execProg newState instrs
    where newState    = (newRegs, newPc)
          newRegs     = case instr of 
            Cpy (Reg reg1) reg2 -> Map.insert reg2 (readReg reg1) regs  
            Cpy (Val val) reg   -> Map.insert reg val regs  
            Inc reg             -> Map.insert reg (readReg reg + 1) regs 
            Dec reg             -> Map.insert reg (readReg reg - 1) regs  
            _                   -> regs 
          newPc       = case instr of 
            Jnz (Reg reg) by    -> if readReg reg == 0 then pc+1 else pc+by
            Jnz (Val val) by    -> if val == 0 then pc+1 else pc+by
            _                   -> pc+1
          instr       = instrs!!pc
          readReg reg = case Map.lookup reg regs of 
            Just val -> val 
            Nothing  -> 0 

main = do 
    input <- (map (parse) . lines) <$> readFile "day12/data/data.in"

    -- Solve part 1 
    let (regs,_) = execProg (Map.empty, 0) input 
    print $ Map.lookup "a" regs

    -- Solve part 2
    let (regs,_) = execProg (Map.singleton "c" 1, 0) input 
    print $ Map.lookup "a" regs