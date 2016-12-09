import Control.Monad.State.Strict

-- checks if the state string is empty
ssIsEmpty :: State String Bool
ssIsEmpty = null <$> get

-- drops n characters from the state string
ssDrop :: Int -> State String ()
ssDrop n = modify (drop n)

-- removes n characters from the state string and returns them
ssTake :: Int -> State String String
ssTake n = do
    (taken, rest) <- splitAt n <$> get
    put rest
    return taken

-- like ssTake but stops at the given Char parameter and discards it
-- "abc(def)" -> ("def)", "abc")
ssTakeUntil :: Char -> State String String
ssTakeUntil c = do
    s <- get
    let (taken, rest) = span (/= c) s

    if null rest
    then put rest
    else put $ tail rest

    return taken

-- PlainMarker decompressor does not decompress the marker data
-- CompressedMarker recursively decompresses marker data
data Decompressor = PlainMarker | CompressedMarker

decompress :: Decompressor -> State String Int
decompress d = do
    empty <- ssIsEmpty

    if empty
    then return 0
    else do
        -- read characters until marker (discards '(')
        ntaken <- length <$> ssTakeUntil '('
        nrest <- decompMarker d
        return $ ntaken + nrest

decompMarker :: Decompressor -> State String Int
decompMarker d = do
    empty <- ssIsEmpty

    if empty
    then return 0
    else do
        -- read count
        strCount <- ssTakeUntil 'x'
        -- read times
        strTimes <- ssTakeUntil ')'

        let count = read strCount
        let times = read strTimes

        case d of
            PlainMarker -> do
                -- ignore marker data
                ssDrop count

                -- continue with plaintext decompression
                nrest <- decompress d

                return $ count * times + nrest

            CompressedMarker -> do
                markerData <- ssTake count

                -- decompress marker data recursively
                let nrest = evalState (decompress d) markerData

                -- continue with plaintext decompression
                nrestp <- decompress d

                return $ times * nrest + nrestp

main :: IO ()
main = do
    contents <- (head . lines) <$> readFile "input.txt"
    print $ evalState (decompress PlainMarker) contents
    print $ evalState (decompress CompressedMarker) contents
