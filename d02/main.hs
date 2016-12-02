import Control.Monad.State

-- row, column coordinate
type Coord = (Int, Int)

-- Keypad description
-- width, height, key characters
-- use '_' for empty spaces
type Keypad = (Int, Int, String)

-- problem type
data Problem = Problem1 | Problem2

instance (Num a, Num b) => Num (a, b) where
    (a1, b1) + (a2, b2) = (a1 + a2, b1 + b2)

-- get keypad character at the given coordinate
keypadIdx :: Keypad -> Coord -> Char
keypadIdx (width, _, lst) (r, c) = lst !! (r * width + c)

initialPosition :: Problem -> Coord
keypad :: Problem -> Keypad

-- problem-specific data
initialPosition Problem1 = (1, 1)
initialPosition Problem2 = (2, 0)

keypad Problem1 = (3, 3,
    "123" ++
    "456" ++
    "789")
keypad Problem2 = (5, 5,
    "__1__" ++
    "_234_" ++
    "56789" ++
    "_ABC_" ++
    "__D__")

-- checks if the position is valid (on the keypad and valid key)
checkPosition :: Problem -> Coord -> Bool
checkPosition p pos@(r, c) = let
        kp@(w, h, _) = keypad p
    in
        r >= 0 && r < h && c >= 0 && c < w && keypadIdx kp pos /= '_'

-- tries to move
applyInstruction :: Problem -> Char -> Coord -> Coord
applyInstruction p c pos = let
        newpos = pos + case c of
            'U' -> (-1,  0)
            'D' -> ( 1,  0)
            'L' -> ( 0, -1)
            'R' -> ( 0,  1)
            _   -> ( 0,  0)
    in
        if checkPosition p newpos
        then newpos
        else pos

-- get the key at the current position
coordToKey :: Problem -> Coord -> Char
coordToKey p = keypadIdx (keypad p)

-- follow the instructions
execInstructions :: Problem -> String -> State Coord ()
execInstructions p instr = do
    pos <- get
    put $ foldl (flip (applyInstruction p)) pos instr

-- follows each set of instructions starting at the initialPosition
-- returns the code
computeKeys :: Problem -> [String] -> String
computeKeys p =
    map (coordToKey p) .
    tail .
    scanl (\s i -> execState (execInstructions p i) s) (initialPosition p)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let instructions = lines contents
    let keys1 = computeKeys Problem1 instructions
    let keys2 = computeKeys Problem2 instructions

    putStrLn keys1
    putStrLn keys2
