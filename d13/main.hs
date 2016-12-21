import Data.Bits (popCount)
import qualified Data.Sequence as S
import qualified Data.Set as DS

puzzleInput :: Int
puzzleInput = 1362

isOpen :: (Int, Int) -> Bool
isOpen (x, y) = x >= 0 && y >= 0 && (even . popCount $ x*x + 3*x + 2*x*y + y + y*y + puzzleInput)

getNeighbours :: (Int, Int) -> S.Seq (Int, Int)
getNeighbours (x, y) = S.filter isOpen $ S.fromList [(x-1, y), (x+1, y), (x, y-1), (x, y+1)]

solveBFS :: (Int, Int) -> Int
solveBFS startpos = let
        helper :: S.Seq ((Int, Int), Int) -> DS.Set (Int, Int) -> Int
        helper queue visited
            | pos == (31, 39) = depth
            | otherwise = let
                    v = S.filter (`DS.notMember` visited) $ getNeighbours pos
                in
                    helper (qs S.>< fmap (\x -> (x, depth + 1)) v) (foldl (flip DS.insert) visited v)
            where ((pos, depth), qs) = (S.index queue 0, S.drop 1 queue)
    in
        helper (S.singleton (startpos, 0)) (DS.singleton startpos)

solveBFS2 :: (Int, Int) -> Int
solveBFS2 startpos = let
        helper :: S.Seq ((Int, Int), Int) -> DS.Set (Int, Int) -> Int
        helper queue visited
            | depth >= 50 = DS.size visited
            | otherwise = let
                    v = S.filter (`DS.notMember` visited) $ getNeighbours pos
                in
                    helper (qs S.>< fmap (\x -> (x, depth + 1)) v) (foldl (flip DS.insert) visited v)
            where ((pos, depth), qs) = (S.index queue 0, S.drop 1 queue)
    in
        helper (S.singleton (startpos, 0)) (DS.singleton startpos)


main :: IO ()
main = do
    print $ solveBFS (1, 1)

    print $ solveBFS2 (1, 1)
