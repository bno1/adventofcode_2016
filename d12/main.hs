import qualified Data.Sequence as S

data Operand =
    Immediate Int |
    Reg Int
    deriving (Show)

data Instruction =
    Copy Operand Operand |
    Inc Operand |
    Dec Operand |
    Jnz Operand Int |
    Noop
    deriving (Show)

-- cpu state described by program counter and register values
data CPUState = CPUState Int (S.Seq Int) deriving (Show)

-- returns the value of a register
getRegValue :: CPUState -> Int -> Int
getRegValue (CPUState _ regs) = S.index regs

-- returns the program counter
getPC :: CPUState -> Int
getPC (CPUState p _) = p

-- applies the first function to the progam counter and the second
-- function to the register referenced by the third argument
modifyCpu :: (Int -> Int) -> (Int -> Int) -> Int -> CPUState -> CPUState
modifyCpu pcf rf r (CPUState p regs) = CPUState (pcf p) (S.adjust rf r regs)

-- initial cpu state (program counter is 0, all registers are 0)
initCPUState :: CPUState
initCPUState = CPUState 0 $ S.replicate 4 0

-- applies the instruction on the cpu
runInstruction :: Instruction -> CPUState -> CPUState
runInstruction (Copy (Immediate v) (Reg r)) cpustate =
    modifyCpu (+1) (const v) r cpustate

runInstruction (Copy (Reg rx) (Reg ry)) cpustate =
    modifyCpu (+1) (const (getRegValue cpustate rx)) ry cpustate

runInstruction (Inc (Reg r)) cpustate =
    modifyCpu (+1) (+1) r cpustate

runInstruction (Dec (Reg r)) cpustate =
    modifyCpu (+1) (subtract 1) r cpustate

runInstruction (Jnz (Immediate rx) p) cpustate =
    if rx /= 0 then
        modifyCpu (+p) id 0 cpustate
    else
        modifyCpu (+1) id 0 cpustate

runInstruction (Jnz (Reg rx) p) cpustate =
    if getRegValue cpustate rx /= 0 then
        modifyCpu (+p) id 0 cpustate
    else
        modifyCpu (+1) id 0 cpustate

runInstruction Noop cpustate =
    modifyCpu (+1) id 0 cpustate

runInstruction instr cpustate = error $ show (show instr, show cpustate)

-- runs the program until the program counter points outside the instruction list
runProgram :: CPUState -> S.Seq Instruction -> CPUState
runProgram cpustate instr
    | pc >= S.length instr = cpustate
    | otherwise = runProgram (runInstruction (S.index instr pc) cpustate) instr
    where pc = getPC cpustate

-- parses an operand (register "a", "b", "c" or "d" or immediate value)
parseOperand :: String -> Operand
parseOperand "a" = Reg 0
parseOperand "b" = Reg 1
parseOperand "c" = Reg 2
parseOperand "d" = Reg 3
parseOperand  i  = Immediate $ read i

parseInstruction :: String -> Instruction
parseInstruction str = case words str of
    ["cpy", x, y] -> Copy (parseOperand x) (parseOperand y)
    ["inc", x] -> Inc (parseOperand x)
    ["dec", x] -> Dec (parseOperand x)
    ["jnz", x, y] -> Jnz (parseOperand x) (read y)
    _ -> Noop

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = S.fromList $ map parseInstruction $ lines contents
    let finalState = runProgram initCPUState instructions

    print finalState

    let initState2 = modifyCpu id (const 1) 2 initCPUState
    let finalState2 = runProgram initState2 instructions

    print finalState2
