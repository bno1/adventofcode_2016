-- computes the next row based on the current one
nextRow :: String -> String
nextRow row = let
        padded = '.':row ++ "."
        depends = zip3 padded (drop 1 padded) (drop 2 padded)
        rule ('^', _, '.') = '^'
        rule ('.', _, '^') = '^'
        rule _ = '.'
    in
        map rule depends

-- counts the number of safe tiles in n rows starting from the given row
countSafeTiles :: String -> Int -> Int
countSafeTiles startRow nrows = let
        countOcc = foldl (\cnt chr -> if chr == '.' then cnt + 1 else cnt)

        helper :: String -> Int -> Int -> Int
        helper _ 0 cnt = cnt
        helper row nrow cnt = helper (nextRow row) (nrow - 1) $! countOcc cnt row
    in
        helper startRow nrows 0

main :: IO ()
main = do
    input <- readFile "input.txt"
    let startRow = head $ lines input

    print $ countSafeTiles startRow 40
    print $ countSafeTiles startRow 400000
