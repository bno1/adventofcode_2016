import Control.Monad.State
import Control.Arrow ((&&&))
import Data.Maybe (isNothing)
import Data.Char (isDigit)
import Data.Set (Set, empty, member, insert)

type Step = (Char, Int)

data Position = Position {
	pos :: (Int, Int),
	dir :: (Int, Int),
	visited :: Set (Int, Int),  -- set of visited positions
	vtwice :: Maybe (Int, Int)  -- first position visited twice
}

initialState :: Position
initialState = Position (0, 0) (1, 0) empty Nothing

-- parses "L2, R5, ..." into [('L', 2), ('R', 5), ...]
parseInput :: String -> [Step]
parseInput =
	map (head &&& read . takeWhile isDigit . tail) . words

-- turns the first argument (vector) by 90 degrees
turn :: (Int, Int) -> Char -> (Int, Int)
turn (dr, dc) 'L' = (dc, -dr)
turn (dr, dc) 'R' = (-dc, dr)
turn d _ = d

-- manhattan distance of a point from initial position
distance :: (Int, Int) -> Int
distance (r, c) = abs (r - ri) + abs (c - ci)
	where (ri, ci) = pos initialState

-- runs an instruction
step :: Step -> State Position (Int, Int)
step (t, d) = let
		-- for every position it checks if it's already in the set
		-- if it is then it stops and returns it
		-- else it inserts it and continues
		updateSet :: [(Int, Int)] -> State (Set (Int, Int)) (Maybe (Int, Int))
		updateSet [] = return Nothing
		updateSet (p:ps) = do
			s <- get
			if member p s then return $ Just p
			else put (insert p s) >> updateSet ps

		-- returns the list of positions visited by following the instruction
		-- p = current pos, dp = direction, d = distance
		computePositions :: (Int, Int) -> (Int, Int) -> Int -> [(Int, Int)]
		computePositions p dp d = tail $
			scanl (\(pr, pc) (pdr, pdc) -> (pr + pdr, pc + pdc)) p $ replicate d dp
	in do
		position <- get
		let (r, c) = pos position
		let (tdr, tdc) = turn (dir position) t
		let fp = (r + tdr * d, c + tdc * d)  -- final pos
		let vt = vtwice position

		modify (\p -> p{pos = fp, dir = (tdr, tdc)})

		-- if no position was visited twice yet
		when (isNothing vt) $ do
			let vps = visited position

			-- compute visited positions by the current instruction
			let positions = computePositions (r, c) (tdr, tdc) d

			-- update the visited set and the visited twice position
			let (newvtwice, newvisited) = runState (updateSet positions) vps

			modify (\p -> p{visited = newvisited, vtwice = newvtwice})

		return fp

main :: IO ()
main = do
	contents <- readFile "input.txt"
	let steps = parseInput contents
	let p = foldl (\a b -> execState (step b) a) initialState steps
	print (distance $ pos p, distance <$> vtwice p)
