import qualified Data.Vector.Unboxed as VU
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.Set as SE
import Data.Char (isDigit)
import Data.List (delete)

-- HVAC map (targets and their positions, width, height, map data as 1D array)
data Map = Map (M.Map Char Int) Int Int (VU.Vector Char) deriving (Show)

-- given a list of rows it generates a map descriptor
parseMap :: [String] -> Map
parseMap rows = Map targets width height vector
    where width = length $ head rows
          height = length rows
          vector = VU.fromList $ concat rows

          -- store the value and position of each character that is digit (target)
          targets = VU.ifoldl (\m i c -> if isDigit c then M.insert c i m else m) M.empty vector

-- returns the list of accessible neighbours
getNeighbours :: Map -> Int -> [Int]
getNeighbours (Map _ width height table) pos = let
        -- vector index to map coordinate
        (py, px) = pos `quotRem` width

        -- map coordinate to vector index
        toIdx (y, x) = y * width + x

        -- check bounds and not wall
        check (y, x) =
            y >= 0 && x >= 0 && y < height && x < width &&
            table VU.! toIdx (y, x) /= '#'
    in
        map toIdx $ filter check [(py-1, px), (py+1, px), (py, px-1), (py, px+1)]

-- computes the minimum distances between all targets
minPaths :: Map -> M.Map (Char, Char) Int
minPaths m@(Map targets _ _ table) = let
        -- BFS that computes the distance to all targets from a starting position
        helper :: S.Seq (Int, Int) -> SE.Set Int -> M.Map Char Int -> M.Map Char Int
        helper queue visited distances
            | S.null queue = distances
            | otherwise = helper
                    -- insert next neighbours in the queue
                    (qs S.>< fmap (\p -> (p, dist + 1)) validNgb)

                    -- add next neighbours to visited nodes
                    (foldl (flip SE.insert) visited validNgb)

                    -- if chr is target add distance to distances map if missing
                    (if isDigit chr && chr `M.notMember` distances
                        then M.insert chr dist distances
                        else distances)
            where ((pos, dist), qs) = (S.index queue 0, S.drop 1 queue)
                  chr = table VU.! pos
                  validNgb = S.fromList $ filter (`SE.notMember` visited) $ getNeighbours m pos

        -- given a starting target and it's position it returns the distances between
        -- it and every target
        runBFS (chr, pos) = M.mapKeys (\chr1 -> (chr, chr1)) $
            helper (S.singleton (pos, 0)) (SE.singleton pos) M.empty
    in
        -- unify the maps of distances for evey node
        foldl M.union M.empty $ map runBFS $ M.toList targets

-- compute the number of steps needed to reach every target from '0' and
-- the number of steps nedded to reach every target from '0' and return to '0'
minCoverageSteps :: Map -> (Int, Int)
minCoverageSteps m@(Map targetsMap _ _ _) = let
        -- compute min distances
        minDistances = minPaths m
        targets = M.keys targetsMap
        minPair (a1, b1) (a2, b2) = (min a1 a2, min b1 b2)

        -- BFS that takes every possible path and returns the minimum no of steps
        helper :: Char -> String -> Int -> (Int, Int)
        helper chr toVisit steps
            | null toVisit = (steps, steps + minDistances M.! (chr, '0'))
            | otherwise = foldl1 minPair $ map nextCall toVisit
            where nextCall nchr = helper
                                    nchr
                                    (delete nchr toVisit)
                                    (steps + minDistances M.! (chr, nchr))
    in
        helper '0' (delete '0' targets) 0

main :: IO ()
main = do
    contents <- readFile "input.txt"
    print $ minCoverageSteps $ parseMap $ lines contents
