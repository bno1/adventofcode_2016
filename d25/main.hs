import Data.Maybe
import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as VU
import qualified Data.Vector.Unboxed.Mutable as VUM
import qualified Data.Map.Strict as M

data Operand =
    Immediate Int |
    Reg Int
    deriving (Show)

data Instruction =
    Copy Operand Operand |
    Inc Operand |
    Dec Operand |
    Out Operand |
    Jnz Operand Operand |
    Noop
    deriving (Show)

-- cpu state described by program counter, transmitted value and register values
data CPUState = CPUState Int Int (VU.Vector Int) deriving (Show, Eq, Ord)

-- returns the value of a register
getRegValue :: CPUState -> Int -> Int
getRegValue (CPUState _ _ regs) = (VU.!) regs

-- returns the program counter
getPC :: CPUState -> Int
getPC (CPUState p _ _) = p

-- applies the first function to the progam counter and the second
-- function to the register referenced by the third argument
modifyCpu :: (Int -> Int) -> (Int -> Int) -> Int -> CPUState -> CPUState
modifyCpu pcf rf r (CPUState p o regs) = CPUState (pcf p) o (VU.modify (\v -> VUM.modify v rf r) regs)

-- increments program coutner
incPC :: CPUState -> CPUState
incPC (CPUState p o regs) = CPUState (p + 1) o regs

-- applies a function on the program coutner
modifyPC :: (Int -> Int) -> CPUState -> CPUState
modifyPC f (CPUState p o regs) = CPUState (f p) o regs

-- initial cpu state (program counter is 0, all registers are 0)
initCPUState :: CPUState
initCPUState = CPUState 0 0 $ VU.replicate 4 0

setOut :: CPUState -> Int -> CPUState
setOut (CPUState pc _ regs) o = CPUState pc o regs

getOut :: CPUState -> Int
getOut (CPUState _ o _) = o

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

runInstruction (Out (Immediate o)) cpustate =
    incPC $ setOut cpustate o

runInstruction (Out (Reg r)) cpustate =
    incPC $ setOut cpustate (getRegValue cpustate r)

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

runInstruction Noop cpustate = incPC cpustate

runInstruction instr cpustate = error $ show (show instr, show cpustate)

-- checks if the given initial state produces the desired clock sequence
-- achieves this by running the program and recording a history of cpu states and
-- outputs and checking the following:
--  1. if the program terminates (program counter outside program) it fails
--  2. if a loop is detected (a cpu state repeats) it succedes only if at least
--     2 outputs were generated and the first output in the loop is 1 - the last
--     output of the loop (the last output after the loop was detected), otherwise
--     it fails
--  3. runs an instruction and saves the current state and time in the state history map.
--     if an output is generated it checks if it's 0 or 1 and different than the
--     previous one and adds the current time and output to the output history map
testState :: V.Vector Instruction -> CPUState -> Bool
testState instructions initialState = let
        -- arguments: time, cpu state, last output, state history map, output history map
        step :: Int -> CPUState -> Int -> M.Map CPUState Int -> M.Map Int Int -> Bool
        step t state lastOut stateHist outHist
            -- check termination
            | pc < 0 || pc >= V.length instructions = False

            -- check loop, firstLoopOut must be 1 - last output of current state
            | loop = firstLoopOut == Just (1 - getOut state)
            | otherwise = case instr of
                -- output is generated, check it and add it to outHist
                Out _ -> (newOut == 0 || newOut == 1) && newOut /= lastOut &&
                         step (t + 1) state' newOut stateHist' (M.insert t newOut outHist)

                -- no output is generated, just continue the program and record history
                _ -> step (t + 1) state' lastOut stateHist' outHist
            where pc = getPC state
                  instr = instructions V.! pc

                  -- next state
                  state' = runInstruction instr state

                  -- new output, used if instruction is Out
                  newOut = getOut state'

                  -- add current state to history
                  stateHist' = M.insert state t stateHist

                  -- lookup current state in history
                  temp = stateHist M.!? state

                  -- true if a loop is detected
                  loop = isJust temp

                  -- time of the first execution of the first instruction in the loop
                  loopTime = fromJust temp

                  -- if a loop is detected this will be the first output generated
                  -- by the loop, the history record says so
                  firstLoopOut = fmap snd $ M.lookupGE loopTime outHist
    in
        step 0 initialState 1 M.empty M.empty

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
    ["out", x] -> Out (parseOperand x)
    ["jnz", x, y] -> Jnz (parseOperand x) (parseOperand y)
    _ -> Noop

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = V.fromList $ map parseInstruction $ lines contents

    -- possible starting states (with different value for reg a)
    let states = map (\a -> modifyCpu id (const a) 0 initCPUState) [0 .. ]

    -- state with smallest positive a that generates the output
    let solution = head $ filter (testState instructions) states

    print $ getRegValue solution 0
