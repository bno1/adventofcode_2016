import Crypto.Hash (Digest, MD5, hash)
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack)
import Data.Word (Word64)
import Data.Char (digitToInt)

md5 :: ByteString -> Digest MD5
md5 = hash

nextChar :: String -> Word64 -> (String, Word64)
nextChar str nounce = do
    let digest = show . md5 $ pack (str ++ show nounce)

    if take 5 digest == "00000"
        then (digest, nounce + 1)
        else nextChar str (nounce + 1)

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

    let digests = tail $ iterate (nextChar door . snd) ("", 0)

    putStrLn "Running phase 1..."

    let answer1 = take 8 $ map ((!! 5) . fst) digests
    mapM_ print answer1
    putStrLn answer1

    putStrLn ""
    putStrLn "Running phase 2..."

    let answer2 = scanl applyDigest (replicate 8 '_') $ map fst digests
    mapM_ putStrLn $ takeWhile (elem '_') answer2
    putStrLn $ head . filter (notElem '_') $ answer2
