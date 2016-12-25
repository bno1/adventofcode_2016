import qualified Data.Sequence as S
import Data.Foldable (toList)

puzzleInput1 :: String
puzzleInput1 = "abcdefgh"

puzzleInput2 :: String
puzzleInput2 = "fbgdceah"

-- types of oprations
data Operation =
    SwapPos Int Int |
    SwapChar Char Char |
    RotateLeft Int |
    RotateRight Int |
    RotateBy Char |
    Reverse Int Int |
    Move Int Int
    deriving (Show)

-- applies the operation on the string
-- I chose a Sequence as string container because it provides operations like S.update
applyOp :: S.Seq Char -> Operation -> S.Seq Char

applyOp s (SwapPos x y) = S.update x cy $ S.update y cx s
    where (cx, cy) = (S.index s x, S.index s y)

-- get indexes and convert to SwapPos
applyOp s (SwapChar cx cy) = applyOp s (SwapPos x y)
    where (Just x, Just y) = (S.elemIndexL cx s, S.elemIndexL cy s)

applyOp s (RotateLeft steps) = S.drop steps' s S.>< S.take steps' s
    where steps' = steps `rem` S.length s

-- convert to RotateLeft
applyOp s (RotateRight steps) = applyOp s (RotateLeft $ S.length s - steps)

-- get index, compute new position and convert to RotateLeft
applyOp s (RotateBy c) = applyOp s (RotateLeft $ 2 * S.length s - x')
    where Just x = S.elemIndexL c s
          x' = x + 1 + if x >= 4 then 1 else 0

-- update each element of the interval
applyOp s (Reverse x y) = foldl (\ss i -> S.update (x + i) (S.index s (y - i)) ss) s [0..y-x]

applyOp s (Move x y) = S.insertAt y c $ S.deleteAt x s
    where c = S.index s x

-- given a list of operations (in reversed order) and a string it returns all
-- the possible unscrambled strings
reverseOp :: [Operation] -> S.Seq Char -> [S.Seq Char]

-- nothing to do
reverseOp [] s = [s]

-- those are straight forward, just mirror the operation
reverseOp (op@(SwapPos _ _):os) s = reverseOp os $ applyOp s op
reverseOp (op@(SwapChar _ _):os) s = reverseOp os $ applyOp s op
reverseOp (RotateLeft steps:os) s = reverseOp os $ applyOp s (RotateRight steps)
reverseOp (RotateRight steps:os) s = reverseOp os $ applyOp s (RotateLeft steps)

-- gets all the indices that could put c at the current position, obtains the
-- unscrambled strings for each possibility and concatenate the solution lists
reverseOp (RotateBy c:os) s = let
        Just ci = S.elemIndexL c s
        isValid x = ci == (2 * x + 1 + if x >= 4 then 1 else 0) `rem` S.length s
        is = filter isValid [0 .. S.length s - 1]
    in
        concatMap (\i -> reverseOp os $ applyOp s (RotateLeft $ S.length s + ci - i)) is

-- those are straight forward too
reverseOp (op@(Reverse _ _):os) s = reverseOp os $ applyOp s op
reverseOp (Move x y:os) s = reverseOp os $ applyOp s (Move y x)

-- parses an operation from the input file
parseOperation :: String -> Operation
parseOperation str = case words str of
    ["swap", "position", x, "with", "position", y] -> SwapPos (read x) (read y)
    ["swap", "letter", x, "with", "letter", y] -> SwapChar (head x) (head y)
    ["rotate", "left", x, _] -> RotateLeft (read x)
    ["rotate", "right", x, _] -> RotateRight (read x)
    ["rotate", "based", "on", "position", "of", "letter", x] -> RotateBy (head x)
    ["reverse", "positions", x, "through", y] -> Reverse (read x) (read y)
    ["move", "position", x, "to", "position", y] -> Move (read x) (read y)
    _ -> error str

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let operations = map parseOperation $ lines contents

    print $ toList $ foldl applyOp (S.fromList puzzleInput1) operations
    print $ map toList $ reverseOp (reverse operations) $ S.fromList puzzleInput2
