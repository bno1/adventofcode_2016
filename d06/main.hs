import Data.Map.Strict (Map, toList, alter, empty)
import Data.List (maximumBy, minimumBy)
import Data.Function (on)

-- takes a string and a list of maps containing occurences for each position
-- adds the occurences of the string to the occurence maps
addOccurences :: String -> [Map Char Int] -> [Map Char Int]
addOccurences = zipWith (alter (Just . maybe 1 (+1)))

-- returns the most frequent character from the occurence map
mostFrequent :: Map Char Int -> Char
mostFrequent = fst . maximumBy (compare `on` snd) . toList

-- returns the least frequent character from the occurence map
leastFrequent :: Map Char Int -> Char
leastFrequent = fst . minimumBy (compare `on` snd) . toList

main :: IO ()
main = do
    signals <- fmap lines $ readFile "input.txt"
    let n = length . head $ signals

    -- compute occurences for each position
    let occurences = foldl (flip addOccurences) (replicate n empty) signals

    -- print most frequent and least frequent signals
    putStrLn $ map mostFrequent occurences
    putStrLn $ map leastFrequent occurences
