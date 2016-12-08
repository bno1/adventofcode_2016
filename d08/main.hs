import Data.List (intercalate)
import Data.Char (isDigit)

-- screen descripto
-- screenData is a list of rows, a row is a list of bools (True = lit pixel)
data Screen = Screen {
    screenWidth :: Int,
    screenHeight :: Int,
    screenData :: [[Bool]]
}

data ScreenCommand =
    Noop           |   -- do nothing
    Rect Int Int   |   -- Rect width height
    RotRow Int Int |   -- Rotate Row idx amount
    RotCol Int Int     -- Rotate Col idx amount
    deriving (Show)

-- prints the screen
instance Show Screen where
    show =
        intercalate "\n" . map (map (\b -> if b then '█' else '░')) . screenData

-- rotates a list to the left
rotateList :: Int -> [a] -> [a]
rotateList _ [] = []
rotateList r l = take (length l) . drop r . cycle $ l

initScreen :: Int -> Int -> Screen
initScreen width height =
    Screen width height (replicate height $ replicate width False)

countLitPixels :: Screen -> Int
countLitPixels = foldl (\c r -> c + (length . filter id $ r)) 0 . screenData

-- applies a command to the screen, returning the updated screen
applyScreenCommand :: ScreenCommand -> Screen -> Screen

applyScreenCommand Noop screen = screen

applyScreenCommand (Rect width' height') screen = let
        -- clamp rect size to screen size
        width = min width' $ screenWidth screen
        height = min height' $ screenHeight screen

        -- separate rows that will be updated from the rest
        (oldrows, keeprows) = splitAt height $ screenData screen

        -- replace the first width elements of each row with lit pixels (True)
        newrows = map ((replicate width True ++) . drop width) oldrows
    in
       screen{screenData = newrows ++ keeprows}

applyScreenCommand (RotRow rowIdx r') screen = let
        sWidth = screenWidth screen
        -- convert r (right rotation) to equivalent left rotation
        r = sWidth - (r' `rem` sWidth)

        -- separates the row that will be rotated from the rest
        (prefix, row:suffix) = splitAt rowIdx $ screenData screen
        newRow = rotateList r row
    in
        screen{screenData = prefix ++ newRow:suffix}

applyScreenCommand (RotCol colIdx r') screen = let
        sHeight = screenHeight screen
        -- convert r (right rotation) to equivalent left rotation
        r = sHeight - (r' `rem` sHeight)

        -- get current column as a list
        oldCol = map (!! colIdx) $ screenData screen
        newCol = rotateList r oldCol

        -- given a row and a pixel it will place the pixel at position colIdx
        processRow row pixel = prefix ++ pixel:suffix
                where (prefix, _:suffix) = splitAt colIdx row
    in
        screen{screenData = zipWith processRow (screenData screen) newCol}

parseCommand :: String -> ScreenCommand
parseCommand str = case words str of
    ["rect", dim] -> Rect (read width) (read height)
        where (width, 'x':height) = span isDigit dim
    ["rotate", "row", idx, "by", rot] -> RotRow (read $ drop 2 idx) (read rot)
    ["rotate", "column", idx, "by", rot] -> RotCol (read $ drop 2 idx) (read rot)
    _ -> Noop

main :: IO()
main = do
    contents <- readFile "input.txt"
    let commands = map parseCommand $ lines contents
    let screen = initScreen 50 6

    let final_screen = foldl (flip applyScreenCommand) screen commands

    print $ countLitPixels final_screen
    putStrLn ""
    print final_screen
