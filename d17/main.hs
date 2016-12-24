import Crypto.Hash (Digest, MD5, hash)
import Data.Char (digitToInt)
import Control.Applicative ((<|>))
import Control.Monad (guard)
import qualified Data.ByteString.Char8 as C
import qualified Data.Sequence as S

-- description of a path (path string and final position)
data Path = Path C.ByteString (Int, Int) deriving (Show)

puzzleInput :: String
puzzleInput = "veumntbg"

md5 :: C.ByteString -> Digest MD5
md5 = hash

-- given a path it returns the sequence of next valid paths
getAvailablePath :: Path -> S.Seq Path
getAvailablePath (Path path (x, y)) = let
        pathHash = show $ md5 path

        -- (char from hash, door taken, updated position)
        pairs = zip3 pathHash "UDLR" [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

        -- checks bounds
        checkPos (nx, ny) = nx >= 0 && nx < 4 && ny >= 0 && ny < 4
    in
        S.fromList [
            Path (path `C.snoc` dir) np |
            (x, y) /= (3, 3),          -- no paths if already at the end
            (c, dir, np) <- pairs,     -- take a pair
            digitToInt c > 10,         -- check if door is open
            checkPos np                -- check bounds
        ]

-- solves the challenge by applying a BFS
solveBFS :: C.ByteString -> (Maybe C.ByteString, Int)
solveBFS passcode = let
        -- walks over all the available paths, recording the shortest one and
        -- the length of the longest
        helper :: S.Seq Path -> Maybe C.ByteString -> Int -> (Maybe C.ByteString, Int)
        helper queue shortest longest
            -- no more paths to explore
            | S.null queue = (finalShortest, finalLongest)
            | otherwise = helper (qs S.>< getAvailablePath p) shortest' longest'
            where (p@(Path path pos), qs) = (queue `S.index` 0, S.drop 1 queue)
                  isEnd = pos == (3, 3)

                  -- if shortest is Nothing and this path isEnd then set to Just path
                  shortest' = shortest <|> fmap (const path) (guard isEnd)

                  -- if this path isEnd and longer than longest update
                  longest' = max longest $ if isEnd then C.length path else 0

                  -- remove passcode part from the path, count only doors taken
                  finalShortest = fmap (C.drop (C.length passcode)) shortest
                  finalLongest = longest - C.length passcode
    in
        helper (S.singleton (Path passcode (0, 0))) Nothing 0

main :: IO ()
main = print $ solveBFS (C.pack puzzleInput)
