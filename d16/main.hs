import qualified Data.Vector.Unboxed as V
import Data.Int (Int8)
import Data.Bits (xor)
import Data.Char (digitToInt, intToDigit)

initialState :: String
initialState = "11100010111110100"

diskLength1 :: Int
diskLength1 = 272

diskLength2 :: Int
diskLength2 = 35651584

-- given a string it returns an equivalent dragon curve
makeDragonCurve :: String -> V.Vector Int8
makeDragonCurve = V.fromList . map (fromIntegral . digitToInt)

-- given a dragon curve it expands it one step
dragonCurve :: V.Vector Int8 -> V.Vector Int8
dragonCurve vec = V.generate (2 * len + 1) helper
    where len = V.length vec
          helper p
            | p < len = vec `V.unsafeIndex` p
            | p == len = 0
            | otherwise = 1 `xor` vec `V.unsafeIndex` (2 * len - p)

-- computes the checksum in O(1) space
checksum :: V.Vector Int8 -> Int -> String
checksum vec len = let
        -- computes the size of the final output (after all reductions) and
        -- how many levels of reductions are needed
        computeLevels :: Int -> Int -> (Int, Int)
        computeLevels size level
            | odd size = (size, level)
            | otherwise = computeLevels (size `quot` 2) (level + 1)

        (size_final, levels) = computeLevels len 0

        -- first argument is the reduction level and second is the position
        -- if reduction level is 0 it reads directly from the dragon curve
        -- otherwise it reads positions 2 * p and 2 * p - 1 from the lower
        -- reduction level and returns 1 if the values are equal, 0 otherwise
        helper :: Int -> Int -> Int8
        helper 0 p = vec `V.unsafeIndex` p
        helper n p = let
                b1 = helper (n - 1) (2 * p)
                b2 = helper (n - 1) (2 * p + 1)
            in
                b1 `xor` b2 `xor` 1
    in
        -- for every character of the output compute the value of the highest
        -- reduction level and convert it to char
        map (intToDigit . fromIntegral . helper levels) [0..size_final - 1]

main :: IO ()
main = do
    -- all curves
    let curves = iterate dragonCurve (makeDragonCurve initialState)

    -- first curve that fills the first disk
    let d1 = head . filter ((>= diskLength1) . V.length) $ curves
    putStrLn $ checksum d1 diskLength1

    -- first curve that fills the second disk
    let d2 = head . filter ((>= diskLength2) . V.length) $ curves
    putStrLn $ checksum d2 diskLength2
