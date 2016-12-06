import Data.List
import Data.Char (isLower, chr, ord)
import Control.Arrow ((&&&))

-- door description
data Door = Door {
    doorName :: [String],
    doorId :: Int,
    doorChecksum :: String
} deriving (Show)

lowercaseLetters :: String
lowercaseLetters = "abcdefghijklmnopqrstuvwxyz"

-- split list at every occurence of an element
-- split "a bcd efgh    ijkl" ' ' => ["a", "bcd", "efgh", "ijkl"]
split :: Eq a => [a] -> a -> [[a]]
split [] _ = []
split lst b
    | head lst == b = split (tail lst) b
    | otherwise = x : split xs b
        where (x, xs) = span (/= b) lst

-- perses a line describing a door
parseDoor :: String -> Door
parseDoor str = let
        parts = split str '-'
        dName = init parts
        dId = read . takeWhile (/= '[') . last $ parts
        dChecksum = filter isLower $ last parts
    in
        Door dName dId dChecksum

-- computes a door's checksum
computeChecksum :: Door -> String
computeChecksum door = let
        -- counts the occurences of chr c in the door name
        countOccurences c = sum . map (length . filter (== c)) $ doorName door

        -- [(chr, occurences of chr in door name)]
        occurences = map (id &&& countOccurences) lowercaseLetters

        -- order by occurence count decresing then by chr increasing
        orderFunc (c1, o1) (c2, o2)
            | o1 == o2 = compare c1 c2
            | otherwise = compare o2 o1
    in
        map fst . take 5 . sortBy orderFunc $ occurences

-- check if the checksum is correct
checkDoor :: Door -> Bool
checkDoor door = computeChecksum door == doorChecksum door

-- shift the character for the shift cipher
shiftChar :: Int -> Char -> Char
shiftChar shift ch =
    chr $ ((ord ch - ord 'a' + shift) `rem` length lowercaseLetters) + ord 'a'

-- decrypt door name
decryptDoor :: Door -> Door
decryptDoor door = let
        dcName = map (map (shiftChar (doorId door))) $ doorName door
    in
        door{doorName = dcName}

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let doors = map parseDoor $ lines contents

    -- filter invalid doors
    let realDoors = filter checkDoor doors

    let answer1 = sum . map doorId $ realDoors
    print answer1

    -- decrypt doors
    let decryptedDoors = map decryptDoor realDoors

    -- find the door containing "northpole" in name
    let northpoleStorage = find (elem "northpole" . doorName) decryptedDoors

    -- print northpoleStorage door name and id
    print $ fmap ((unwords . doorName) &&& doorId) northpoleStorage
