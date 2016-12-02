import Control.Monad.State
import Control.Arrow ((&&&))
import Data.Maybe (isNothing)
import Data.Char (isDigit)
import Data.Set (Set, empty, member, insert)

type Instruction = (Char, Int)
type Coord = (Int, Int)

data PState = PState {
    pstatePos :: Coord,
    pstateDir :: Coord,
    pstateVisited :: Set Coord,  -- set of visited positions
    pstateVTwice :: Maybe Coord  -- first position visited twice
}

instance (Num a, Num b) => Num (a, b) where
    (a1, b1) + (a2, b2) = (a1 + a2, b1 + b2)
    (a1, b1) * (a2, b2) = (a1 * a2, b1 * b2)

initialState :: PState
initialState = PState (0, 0) (1, 0) empty Nothing

-- parses "L2, R5, ..." into [('L', 2), ('R', 5), ...]
parseInput :: String -> [Instruction]
parseInput = map (head &&& read . takeWhile isDigit . tail) . words

-- turns the first argument (vector) by 90 degrees
turn :: Coord -> Char -> Coord
turn (dr, dc) 'L' = (dc, -dr)
turn (dr, dc) 'R' = (-dc, dr)
turn d _ = d

-- manhattan distance of a point from initial position
distance :: Coord -> Int
distance pos = abs r + abs c
    where (r, c) = pos + pstatePos initialState

-- returns the list of positions visited by following the instruction
visitedPositions :: Coord -> Coord -> Int -> [Coord]
visitedPositions pos dir dist = tail . scanl (+) pos . replicate dist $ dir

-- runs an instruction
step :: Instruction -> State PState Coord
step (t, dist) = let
        -- for every position it checks if it's already in the set
        -- if it is then it stops and returns it
        -- else it inserts it and continues
        updateSet :: [Coord] -> State (Set Coord) (Maybe Coord)
        updateSet [] = return Nothing
        updateSet (pos:ps) = do
            set <- get
            if member pos set
                then return $ Just pos
                else put (insert pos set) >> updateSet ps
    in do
        pstate <- get
        let pos = pstatePos pstate
        let dir' = turn (pstateDir pstate) t
        let pos' = pos + dir' * (dist, dist)  -- final pos

        modify (\p -> p{pstatePos = pos', pstateDir = dir'})

        -- if no pstate was visited twice yet
        when (isNothing $ pstateVTwice pstate) $ do
            -- compute visited positions by the current instruction
            let visited = visitedPositions pos dir' dist

            -- update the visited set and the visited twice position
            let vps = pstateVisited pstate
            let (vtwice', visited') = runState (updateSet visited) vps

            modify (\p -> p{pstateVisited = visited', pstateVTwice = vtwice'})

        return pos'

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let steps = parseInput contents
    let pstate = foldl (\a b -> execState (step b) a) initialState steps
    print (distance $ pstatePos pstate, distance <$> pstateVTwice pstate)
