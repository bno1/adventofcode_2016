import Control.Monad.State

-- ip descriptor that separates the supernet and hypernet parts
data IPParts = IPParts {
    ipSupernet :: [String],   -- supernet parts
    ipHypernet :: [String]    -- hypernet parts
} deriving (Show)

-- appends a string to the supernet list
ippAddSupernet :: String -> IPParts -> IPParts
ippAddSupernet s (IPParts ips iph) = IPParts (ips ++ [s]) iph

-- appends a string to the hypernet list
ippAddHypernet :: String -> IPParts -> IPParts
ippAddHypernet s (IPParts ips iph) = IPParts ips (iph ++ [s])

isABBA :: String -> Bool
isABBA (a:b:c:d:_) = a == d && b == c && a /= b
isABBA _ = False

isABA :: String -> Bool
isABA (a:b:c:_) = a == c && a /= b
isABA _ = False

convertABAToBAB :: String -> String
convertABAToBAB [a, b, _] = [b, a, b]
convertABAToBAB _ = ""

-- returns all consecutive subsequences of length 4 in the string
subseq4 :: String -> [String]
subseq4 str@(_:_:_:_:_) = take 4 str : subseq4 (tail str)
subseq4 _ = []

-- returns all consecutive subsequences of length 3 in the string
subseq3 :: String -> [String]
subseq3 str@(_:_:_:_) = take 3 str : subseq3 (tail str)
subseq3 _ = []

stringHasABBA :: String -> Bool
stringHasABBA = any isABBA . subseq4

-- returns the list of ABA sequences
stringABAs :: String -> [String]
stringABAs = filter isABA . subseq3

-- checks if ip respects the TLS requirement
checkIPTLS :: IPParts -> Bool
checkIPTLS ipp =
    -- no ABBA in hypernet
    not (any stringHasABBA $ ipHypernet ipp) &&
    -- at least one ABBA in supernet
    any stringHasABBA (ipSupernet ipp)

-- checks if ip respects the SSL requirement
checkIPSSL :: IPParts -> Bool
checkIPSSL ipp = let
        -- ABAs in supernet
        abas = concatMap stringABAs $ ipSupernet ipp
        -- BABs in hypernet
        babs = concatMap stringABAs $ ipHypernet ipp
    in
        -- check if an ABA from supernet <=> BAB from hypernet
        any (`elem` babs) $ map convertABAToBAB abas

-- parses a string as an IPParts
parseIP :: String -> IPParts
parseIP str = let
        -- reads the supernet sequence until it finds a [
        -- then calls readHypernet to continue
        readSupernet :: String -> String -> State IPParts ()
        readSupernet "" s = unless (null s) $ modify (ippAddSupernet s)
        readSupernet ('[':xs) s = do
            readSupernet "" s
            readHypernet xs ""

        readSupernet (x:xs) s = readSupernet xs (s ++ [x])

        -- reads the hypernet sequence until it find a [
        -- then calls readSupernet to continue
        readHypernet :: String -> String -> State IPParts ()
        readHypernet "" s = unless (null s) $ modify (ippAddHypernet s)
        readHypernet (']':xs) s = do
            readHypernet "" s
            readSupernet xs ""

        readHypernet (x:xs) s = readHypernet xs (s ++ [x])
    in
        execState (readSupernet str "") (IPParts [] [])

main :: IO ()
main = do
    content <- readFile "input.txt"
    let ips = map parseIP $ lines content

    let tls_ips = filter checkIPTLS ips
    print $ length tls_ips

    let ssl_ips = filter checkIPSSL ips
    print $ length ssl_ips
