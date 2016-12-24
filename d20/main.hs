import qualified Data.Set as S
import Data.Maybe (mapMaybe)
import Data.Int (Int64)

type IP = Int64

maxIP :: IP
maxIP = 4294967295

-- an ip intervaled described by it's low end and high end
data Interval = Interval IP IP deriving (Show)

-- two intervals are equal if they overlap
instance Eq Interval where
    (Interval l1 h1) /= (Interval l2 h2) = h1 < l2 || h2 < l1
    (Interval l1 h1) == (Interval l2 h2) = h1 >= l2 && h2 >= l1

-- intervals are ordered by their order on the real number axis
instance Ord Interval where
    compare (Interval l1 h1) (Interval l2 h2)
        | h1 < l2 = LT
        | h2 < l1 = GT
        | otherwise = EQ

-- returns the high end of the interval
getHigh :: Interval -> IP
getHigh (Interval _ h) = h

-- makes an interval out of 2 ips
-- arguments don't have to be ordered
makeInterval :: IP -> IP -> Interval
makeInterval x y = Interval (min x y) (max x y)

-- combines (by union) two overlapping intervals
combineIntervals :: Interval -> Interval -> Interval
combineIntervals (Interval l1 h1) (Interval l2 h2) = Interval (min l1 l2) (max h1 h2)

-- parses an interval from the input line
parseInterval :: String -> Maybe Interval
parseInterval str = case break (=='-') str of
    (low, '-':high) -> Just $ makeInterval (read low) (read high)
    _ -> Nothing

-- given a list of intervals it combines all the overlapping intervals
-- the returned list describes the same ip ranges but contains no overlapping intervals
normalizeIntervals :: [Interval] -> [Interval]
normalizeIntervals [] = []
normalizeIntervals (x:xs)
    -- if there are intervals overlapping to x combine them with x
    -- and remove x from list
    | x `elem` xs = normalizeIntervals $ map (\i -> if x == i then combineIntervals x i else i) xs
    -- no overlaps, leave x in list
    | otherwise = x : normalizeIntervals xs

-- find the smalles ip (>= 0) that is allowed by the black list
minAllowed :: S.Set Interval -> IP
minAllowed set = let
        helper :: IP -> IP
        -- S.lookupIndex will return (Just n) if an interval overlapps with [ip, ip]
        -- if it returns Nothing then no rule blocks that ip
        -- otherwise take the high end of the rule's interval, add one and check again
        helper ip = maybe ip nextcall index

            where index = S.lookupIndex (makeInterval ip ip) set
                  nextcall = helper . (+1) . getHigh . flip S.elemAt set
    in
        helper 0

-- returns the number of ips that are allowed by the black list
-- it does this by iterating over the invervals (in sorted order) and summing the
-- distances between them and 0 and maxIP
countAllowed :: S.Set Interval -> Int64
countAllowed set = maxIP - high + count
    where (Interval _ high) = S.findMax set
          (count, _) = foldl (\(cnt, p) (Interval l h) -> (cnt + l - p - 1, h)) (0, -1) set

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let intervals = mapMaybe parseInterval $ lines contents

    -- intervals must be normalized, otherwise overlapping intervals will repleace
    -- one another when the set is constructed
    let blacklist = S.fromList $ normalizeIntervals intervals

    print $ minAllowed blacklist
    print $ countAllowed blacklist
