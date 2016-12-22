import Data.Bits (popCount)
import Data.Maybe (isJust)
import qualified Data.Sequence as S
import qualified Data.Set as DS
import Control.Monad

-- problem inputs
favoriteNumber :: Int
favoriteNumber = 1362

targetPos :: (Int, Int)
targetPos = (31, 39)

targetDepth :: Int
targetDepth = 50

-- checks if a position is a wall or open space
isOpen :: (Int, Int) -> Bool
isOpen (x, y) =
    x >= 0 &&
    y >= 0 &&
    (even . popCount $ x*x + 3*x + 2*x*y + y + y*y + favoriteNumber)

-- returns the list of valid neighbour positions
getNeighbours :: (Int, Int) -> [(Int, Int)]
getNeighbours (x, y) = filter isOpen [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

-- given a start position and a test function it runs a BFS on the map
-- the test function receives the current position, its depth and the set of
-- visited positions so far and returns a Maybe a
-- if the test function returns a Just at a position then solveBFS will stop and
-- return it
-- solveBFS returns Nothing if all the possible positions are explored and at no
-- position the test function returns a Just
solveBFS :: (Int, Int) -> ((Int, Int) -> Int -> DS.Set (Int, Int) -> Maybe a) -> Maybe a
solveBFS startpos testfunc = let
        helper queue visited
            | S.null queue = Nothing
            | isJust r = r
            | otherwise = let
                    validNgb = S.fromList . filter (`DS.notMember` visited) $ getNeighbours pos
                in
                    helper
                        (qs S.>< fmap (\x -> (x, depth + 1)) validNgb)
                        (foldl (flip DS.insert) visited validNgb)
            where ((pos, depth), qs) = (S.index queue 0, S.drop 1 queue)
                  r = testfunc pos depth visited
    in
        helper (S.singleton (startpos, 0)) (DS.singleton startpos)

main :: IO ()
main = do
    -- explore map until it reaches the target position and returns its depth
    print $ solveBFS (1, 1)
        (\pos depth _ -> guard (pos == targetPos) >> return depth)

    -- explore map until it reaches the target depth and returns the number of visited positions
    print $ solveBFS (1, 1)
        (\_ depth visited -> guard (depth >= targetDepth) >> return (DS.size visited))
