import qualified Data.Vector as V
import qualified Data.Vector.Mutable as VM
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM

data Operand =
    Immediate Int |
    Reg Int
    deriving (Show)

data Instruction =
    Copy Operand Operand |
    Inc Operand |
    Dec Operand |
    Jnz Operand Operand |
    Toggle Operand |
    Noop
    deriving (Show)

-- cpu state described by program counter and register values
data CPUState = CPUState Int (VU.Vector Int) deriving (Show)

-- returns the value of a register
getRegValue :: CPUState -> Int -> Int
getRegValue (CPUState _ regs) = (VU.!) regs

-- returns the program counter
getPC :: CPUState -> Int
getPC (CPUState p _) = p

-- applies the first function to the progam counter and the second
-- function to the register referenced by the third argument
modifyCpu :: (Int -> Int) -> (Int -> Int) -> Int -> CPUState -> CPUState
modifyCpu pcf rf r (CPUState p regs) = CPUState (pcf p) (VU.modify (\v -> VUM.modify v rf r) regs)

-- increment program counter
incPC :: CPUState -> CPUState
incPC (CPUState p regs) = CPUState (p + 1) regs

-- apply a function on the program counter
modifyPC :: (Int -> Int) -> CPUState -> CPUState
modifyPC f (CPUState p regs) = CPUState (f p) regs

-- initial cpu state (program counter is 0, all registers are 0)
initCPUState :: CPUState
initCPUState = CPUState 0 $ VU.replicate 4 0

-- applies the instruction on the cpu
runInstruction :: Instruction -> CPUState -> CPUState
runInstruction (Copy (Immediate _) (Immediate _)) cpustate = incPC cpustate

runInstruction (Copy (Immediate v) (Reg r)) cpustate =
    modifyCpu (+1) (const v) r cpustate

runInstruction (Copy (Reg rx) (Reg ry)) cpustate =
    modifyCpu (+1) (const (getRegValue cpustate rx)) ry cpustate

runInstruction (Inc (Reg r)) cpustate =
    modifyCpu (+1) (+1) r cpustate

runInstruction (Dec (Reg r)) cpustate =
    modifyCpu (+1) (subtract 1) r cpustate

runInstruction (Jnz (Immediate rx) (Immediate p)) cpustate =
    if rx /= 0 then
        modifyPC (+p) cpustate
    else
        incPC cpustate

runInstruction (Jnz (Immediate rx) (Reg ry)) cpustate =
    if rx /= 0 then
        modifyPC (+ getRegValue cpustate ry) cpustate
    else
        incPC cpustate

runInstruction (Jnz (Reg rx) (Immediate p)) cpustate =
    if getRegValue cpustate rx /= 0 then
        modifyPC (+p) cpustate
    else
        incPC cpustate

runInstruction (Jnz (Reg rx) (Reg ry)) cpustate =
    if getRegValue cpustate rx /= 0 then
        modifyPC (+ getRegValue cpustate ry) cpustate
    else
        incPC cpustate

runInstruction (Toggle _) cpustate = incPC cpustate

runInstruction Noop cpustate = incPC cpustate

runInstruction instr cpustate = error $ show (show instr, show cpustate)

-- toggles the instruction pointed by the operand
applyToggle :: Operand -> CPUState -> V.Vector Instruction -> V.Vector Instruction
applyToggle (Immediate p) cpustate instructions = let
        modifyInstr :: Instruction -> Instruction
        modifyInstr (Copy x y) = Jnz x y
        modifyInstr (Jnz x y) = Copy x y
        modifyInstr (Inc x) = Dec x
        modifyInstr (Dec x) = Inc x
        modifyInstr (Toggle x) = Inc x
        modifyInstr Noop = Noop

        ip = getPC cpustate + p
    in
        if ip < 0 || ip >= V.length instructions
        then instructions
        else V.modify (\v -> VM.modify v modifyInstr ip) instructions

applyToggle (Reg r) cpustate instructions =
    applyToggle (Immediate $ getRegValue cpustate r) cpustate instructions

-- runs the program until the program counter points outside the instruction list
runProgram :: CPUState -> V.Vector Instruction -> CPUState
runProgram cpustate instructions
    | pc < 0 || pc >= V.length instructions = cpustate
    | otherwise =
        case instr of
        (Toggle o) -> runProgram cpustate' $ applyToggle o cpustate instructions
        _ -> runProgram cpustate' instructions
    where pc = getPC cpustate
          instr = instructions V.! pc
          cpustate' = runInstruction instr cpustate

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
    ["jnz", x, y] -> Jnz (parseOperand x) (parseOperand y)
    ["tgl", x] -> Toggle (parseOperand x)
    _ -> Noop

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = V.fromList $ map parseInstruction $ lines contents

    -- reg a is 7
    let initialState1 = modifyCpu id (const 7) 0 initCPUState
    let finalState = runProgram initialState1 instructions

    print finalState

    -- reg a is 12
    let initState2 = modifyCpu id (const 12) 0 initCPUState
    let finalState2 = runProgram initState2 instructions

    print finalState2
