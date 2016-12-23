import Data.Char (isAlphaNum)
import Data.Maybe (mapMaybe)

-- disc decription (number of positions and current position)
data Disc = Disc Int Int

-- parses a line from the input file
parseDisc :: String -> Maybe Disc
parseDisc str = case map (filter isAlphaNum) $ words str of
    ["Disc", _, "has", p, "positions", "at", "time0", "it", "is", "at", "position", q]
        -> Just $ Disc (read p) (read q)
    _ -> Nothing

solve :: [Disc] -> Int
solve [] = error "no discs"
solve (Disc fps fp:discs) = let
        -- checks if the capsule falls through every disc
        checker :: [Disc] -> Int -> Bool
        checker [] _ = True
        checker (Disc ps p:ds) t
            | (p + t + 1) `rem` ps == 0 = checker ds (t + 1)
            | otherwise = False

        -- determine the earliest time to press the button in order for the
        -- capsule to fall through the first disc
        start_time = (2 * fps - fp - 1) `rem` fps
    in
        -- for every position that causes the capsule to fall through the first
        -- disc check if the capsule falls though every disc
        head $ filter (checker discs . (+1)) [start_time, start_time + fps ..]

main :: IO ()
main = do
    contents <- readFile "input.txt"

    let discs1 = mapMaybe parseDisc $ lines contents
    print $ solve discs1

    let discs2 = discs1 ++ [Disc 11 0]
    print $ solve discs2
