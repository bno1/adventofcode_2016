import Crypto.Hash (Digest, MD5, hash)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Word (Word64)
import Data.Char (digitToInt)

-- compute md5 of a bytestring
md5 :: ByteString -> Digest MD5
md5 = hash

-- takes a string and a starting nounce
-- finds the first nounce such that md5(string + nounce) starts with 5 zeros
-- returns the md5 digest and the found nounce + 1
nextDigest :: String -> Word64 -> (String, Word64)
nextDigest str nounce = let
        digest = show . md5 $ pack (str ++ show nounce)
    in if take 5 digest == "00000"
        then (digest, nounce + 1)
        else nextDigest str (nounce + 1)

-- takes a string and a md5 digest, returns a modified string
-- p = digest[5], c = digest[6]
-- if string[p] =/= '_' then string[p] = c
applyDigest :: String -> String -> String
applyDigest str digest = let
        p = digitToInt $ digest !! 5
        c = digest !! 6
        (prefix, _:suffix) = splitAt p str
    in
        if 0 <= p && p < length str && str !! p == '_'
            then prefix ++ c : suffix
            else str

main :: IO()
main = do
    door <- fmap (head . lines) $ readFile "input.txt"

    -- list of all digests of type "door name + nounce" that start with 5 zeros
    let digests = tail $ iterate (nextDigest door . snd) ("", 0)

    putStrLn "Running phase 1..."

    -- generate characters for first answer, can take a while
    let answer1 = take 8 $ map ((!! 5) . fst) digests

    -- prints characters as they are generated
    mapM_ print answer1
    -- prints the final complete answer
    putStrLn answer1

    putStrLn ""
    putStrLn "Running phase 2..."

    -- generate string for answer 2, can take a while
    let answer2 = scanl applyDigest (replicate 8 '_') $ map fst digests

    -- prints the answer as it is generated (while it still contains '_'s)
    mapM_ putStrLn $ takeWhile (elem '_') answer2
    -- print the final answer
    putStrLn $ head . filter (notElem '_') $ answer2
