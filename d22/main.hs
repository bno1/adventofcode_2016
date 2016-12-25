import qualified Data.Set as SE
import qualified Data.Vector.Unboxed as V
import qualified Data.Sequence as S
import Data.Char (isDigit)
import Data.Maybe (fromMaybe)

-- position on the grid (y, x)
type Pos = (Int, Int)

-- description of a node (capacity, used, available)
type NodeInfo = (Int, Int, Int)

-- cluster description (width, hight, vector or nodes)
data Cluster = Cluster Int Int (V.Vector NodeInfo) deriving (Show)

-- creates the cluster description from a list of positions and nodes descriptors
createCluster :: [(Pos, NodeInfo)] -> Cluster
createCluster nodes = Cluster width height $ V.generate (length nodes) get
    where width  = 1 + foldl (\x -> max x . snd . fst) 0 nodes
          height = 1 + foldl (\x -> max x . fst . fst) 0 nodes
          get idx = fromMaybe (error $ show idx) $ lookup (quotRem idx width) nodes

-- turn (y, x) into a vector index and read it
getNode :: Cluster -> Pos -> NodeInfo
getNode (Cluster width _ nodes) (y, x) = nodes V.! (y * width + x)

-- returns the neighbour nodes that could be moved in the given node if it would be empty
getMoves :: Cluster -> Pos -> [Pos]
getMoves cluster@(Cluster width height _) p@(py, px) = let
        (ssize, _, _) = getNode cluster p
        checkBounds (y, x) = x >= 0 && y >= 0 && x < width && y < height
        checkMove pd = ssize >= dusage
            where (_, dusage, _) = getNode cluster pd

        neighbours = filter checkBounds [(py+1, px), (py-1, px), (py, px+1), (py, px-1)]
    in
        filter checkMove neighbours

-- counts the number of movable node pairs
countNodePairs :: Cluster -> Int
countNodePairs (Cluster _ _ nodes) = V.ifoldl (\cnt i1 (_, u, _) ->
        if u == 0
        then cnt
        else cnt + V.ifoldl (\cnt2 i2 (_, _, a) ->
                if i1 /= i2 && a >= u
                then cnt2 + 1
                else cnt2
            ) 0 nodes
    ) 0 nodes

-- use a BFS to compute the distance from the empty node to the target node
distToTarget :: Cluster -> Int
distToTarget cluster@(Cluster width _ nodes) = let
        initialTarget = (0, width - 1)

        -- index of empty node
        Just initialZeroIdx = V.findIndex (\(_, u, _) -> u == 0) nodes
        initialZero = initialZeroIdx `quotRem` width

        helper :: S.Seq (Pos, Int) -> SE.Set Pos -> Int
        helper queue visited
            | pos == initialTarget = dist
            | otherwise = helper
                (qs S.>< fmap (\s -> (s, dist + 1)) validMoves)
                (foldl (flip SE.insert) visited validMoves)
            where ((pos, dist), qs) = (S.index queue 0, S.drop 1 queue)
                  moves = getMoves cluster pos
                  validMoves = S.fromList $ filter (`SE.notMember` visited) moves
    in
        helper (S.singleton (initialZero, 0)) (SE.singleton initialZero)

-- compute the minimum number of moves needed to get the target node's data
-- the movement algorithm works as follows:
--   1. move the empty node in the position of the target node, moving the
--      target node one position to the left
--   2. move the empty node around the target node and bring it one position left
--      (5 moves) until it reaches position (0, 0) (so width - 2 repetitions)
--
-- we need a BFS for step 1 in order to avoid huge full nodes
-- the second step is pretty dumb, it assumes that no huge nodes are found on
-- the first two rows and that every node's data can fit in another node if it's empty
--
-- the general case is very difficult to solve and I assume that all the inputs
-- are crafted in order to work with this dumb algorithm
minMoves :: Cluster -> Int
minMoves cluster@(Cluster width _ _) = distToTarget cluster + 5 * (width - 2)

-- prints the grid in the same ". _ #" notation that they use
-- it uses a threshold of 100 to separate huge nodes from movable nodes, this might
-- not be right for your input
showMap :: Cluster -> [String]
showMap cluster@(Cluster width height _) = map (\y -> map (func y) [0 .. width - 1]) [0 .. height - 1]
    where func y x
            | u > 100 = '#'
            | u == 0 = '_'
            | otherwise = '.'
            where (_, u, _) = getNode cluster (y, x)

-- parses a string into a (Pos, NodeInfo)
parseNode :: String -> (Pos, NodeInfo)
parseNode str = case words $ map (\c -> if isDigit c then c else ' ') str of
    [x, y, s, u, a, _] -> ((read y, read x), (read s, read u, read a))
    _ -> error str

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let nodes = map parseNode $ drop 2 $ lines contents
    let cluster = createCluster nodes

    -- print map
    mapM_ putStrLn $ showMap cluster

    print $ countNodePairs cluster
    print $ minMoves cluster
