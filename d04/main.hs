import Data.List
import Data.Char (isLower, chr, ord)
import Control.Arrow ((&&&))

data Door = Door {
    doorName :: [String],
    doorId :: Int,
    doorChecksum :: String
} deriving (Show)

lowercaseLetters :: String
lowercaseLetters = "abcdefghijklmnopqrstuvwxyz"

split :: Eq a => [a] -> a -> [[a]]
split [] _ = []
split lst b
    | head lst == b = split (tail lst) b
    | otherwise = x : split xs b
        where (x, xs) = span (/= b) lst

parseDoor :: String -> Door
parseDoor str = let
        parts = split str '-'
        dName = init parts
        dId = read . takeWhile (/= '[') . last $ parts
        dChecksum = filter isLower $ last parts
    in
        Door dName dId dChecksum

computeChecksum :: Door -> String
computeChecksum door = let
        countOccurences c = sum . map (length . filter (== c)) $ doorName door
        occurences = map (id &&& countOccurences) lowercaseLetters
        orderFunc (c1, o1) (c2, o2)
            | o1 == o2 = compare c1 c2
            | otherwise = compare o2 o1
    in
        map fst . take 5 . sortBy orderFunc $ occurences

checkDoor :: Door -> Bool
checkDoor door = computeChecksum door == doorChecksum door

shiftChar :: Int -> Char -> Char
shiftChar shift ch =
    chr $ ((ord ch - ord 'a' + shift) `rem` length lowercaseLetters) + ord 'a'

decryptDoor :: Door -> Door
decryptDoor door = let
        dcName = map (map (shiftChar (doorId door))) $ doorName door
    in
        door{doorName = dcName}

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let doors = map parseDoor $ lines contents
    let realDoors = filter checkDoor doors

    let answer1 = sum . map doorId $ realDoors
    print answer1

    let decryptedDoors = map decryptDoor realDoors
    let northpoleStorage = find (elem "northpole" . doorName) decryptedDoors

    print $ fmap ((unwords . doorName) &&& doorId) northpoleStorage
