import Data.Char (isAlphaNum)
import Data.List (elemIndex, sortOn)
import Data.Maybe (fromMaybe)
import Data.Bits (xor, (.|.), (.&.), unsafeShiftL, unsafeShiftR, shift)
import Data.Int (Int64)
import Data.Hashable
import qualified Data.Map.Strict as M
import qualified Data.Sequence as S
import qualified Data.HashSet as HS

-- an item (generator or microchip) is represented as a single bit
-- generators have even positions while microchips have the next odd positions
-- for example:
--   thorium generator -> bit #0 (0x1)
--   thorium microchip -> bit #1 (0x2)
--   plutonium generator -> bit #2 (0x4)
--   plutonium microchip -> bit #3 (0x8)
type Item = Int64

-- an item set is a group (bitwise or) of multiple items
-- thorium generator and thorium microchip and plutonium microchip
--     = 0x1 | 0x2 | 0x8 = 0xb
type ItemSet = Int64

-- the state represents the current floor (first Int) and the sets for all floors
-- the sets for the floors are all stored in a single Int64 type
-- since there are always 4 floors it means that there is space for 64 / 4 / 2 =
--     8 types of elements
-- this is more than enough for this problem
data State = State
    {-# UNPACK #-} !Int
    {-# UNPACK #-} !ItemSet
    deriving (Show, Eq, Ord)

-- needed for compatibility with Data.HashSet
instance Hashable State where
    hashWithSalt s (State f v) = s `xor` f `xor` fromIntegral v
    hash (State f v) = f `xor` fromIntegral v

-- number of bits per floor in the State
stateFloorSize :: Int
stateFloorSize = 16

-- mask for a floor in the State
stateFloorMask :: ItemSet
stateFloorMask = 0xffff

-- iinitialize state with floor number and list of floor item sets
initState :: Int -> [ItemSet] -> State
initState f = State f . foldl xor 0 .
    zipWith (\i v -> v `unsafeShiftL` (i * stateFloorSize)) [0..]

-- returns the current floor number
getFloor :: State -> Int
getFloor (State f _) = f

-- returns the current floor items
getFloorItems :: State -> ItemSet
getFloorItems s@(State f _) = getFloorItems' s f

-- returns the floor items of the given floor
getFloorItems' :: State -> Int -> ItemSet
getFloorItems' (State _ v) f =
    fromIntegral $ v `unsafeShiftR` (f * stateFloorSize) .&. stateFloorMask

-- updates the floor sets and increments the floor number
-- is represents the item set of items that will move
-- warning: it doesn't check if the move is valid by any means
moveUp :: State -> ItemSet -> State
moveUp (State f v) is = let
        -- shift for the current floor
        shift1 = f * stateFloorSize
        -- shift for the upper floor
        shift2 = shift1 + stateFloorSize
    in
        State (f + 1) (v `xor` (is `unsafeShiftL` shift1) `xor` (is `unsafeShiftL` shift2))

-- like moveUp
moveDown :: State -> ItemSet -> State
moveDown (State f v) is = let
        shift1 = f * stateFloorSize
        shift2 = shift1 - stateFloorSize
    in
        State (f - 1) (v `xor` (is `unsafeShiftL` shift1) `xor` (is `unsafeShiftL` shift2))

-- checks if the state is final (elevator at 4th floor and all floors below 4 empty)
stateIsFinal :: State -> Bool
stateIsFinal (State f v) = f == 3 && v .&. 0xffffffffffff == 0

-- given an item set and a start item it returns a sequence of all the individual
-- items in the set
expandItemSet :: ItemSet -> Item -> S.Seq ItemSet
expandItemSet v p
    | p > v = S.empty
    | v .&. p /= 0 = p S.<| expandItemSet v (p `unsafeShiftL` 1)
    | otherwise = expandItemSet v (p `unsafeShiftL` 1)

-- builds an item set based item list
combineItems :: [Item] -> ItemSet
combineItems = foldl (.|.) 0

-- parses a list of words into a list of items descriptions
-- returns list of items like ('G', generator_name) or ('M', microchip_name)
parseItemList :: [String] -> [(Char, String)]
parseItemList [] = []
parseItemList ["nothing", "relevant"] = []
parseItemList ("and":xs) = parseItemList xs
parseItemList ("a":name:"microchip":xs) =
    ('M', take (length name - 10) name) : parseItemList xs
parseItemList ("a":name:"generator":xs) =
    ('G', name) : parseItemList xs
parseItemList p = error $ show p

-- parses a line from the input file
-- returns a (floor_number, list_of_items)
parseFloor :: String -> (Int, [(Char, String)])
parseFloor "" = (-1, [])
parseFloor str = let
        -- remove punction marks and "and"
        parts = filter (/="and") . map (filter isAlphaNum) $ words str
        floorno = fromMaybe (-1) $
            elemIndex (parts !! 1) ["first", "second", "third", "fourth"]
    in
        (floorno, parseItemList $ drop 4 parts)

-- converts the result of parseFloor into a numberical representation of
-- items conforming with the description of ItemSet
-- returns a pair containing the list of items on each floor and the map of
-- name -> bit pos / 2 associations
dataToNumeric :: [[(Char, String)]] -> ([[Item]], M.Map String Int)
dataToNumeric lst = let
        -- assigns each element a unique number starting from 0
        helper :: [(Char, String)] -> M.Map String Int -> M.Map String Int
        helper [] m = m
        helper ((_, name):xs) m = case M.lookup name m of
            Just _ -> helper xs m
            Nothing -> helper xs (M.insert name (M.size m) m)

        names = helper (concat lst) M.empty

        -- converts an item generated by parseItemList into a Item
        convert :: (Char, String) -> Item
        convert ('G', name) = 1 `shift` (2 * M.findWithDefault 0 name names)
        convert ('M', name) = 1 `shift` (1 + 2 * M.findWithDefault 0 name names)
        convert x = error $ show x
    in
        (map (map convert) lst, names)

-- checks if the state is valid (no floor has an unshielded microchip and at
-- least one generator)
checkState :: State -> Bool
checkState state = let
        -- first bool of pair is true if an unshielded microchip exists
        -- second bool of pair is true if at least one generator exists
        checkIS :: ItemSet -> (Bool, Bool) -> Bool
        checkIS 0 (mc, g) = not $ mc && g
        checkIS _ (True, True) = False
        checkIS i (mc, g) =
            -- the way item sets are encoded allows very easy check for
            -- unshielded microchip
            checkIS (i `unsafeShiftR` 2) (mc || i .&. 3 == 2, g || i .&. 1  == 1)
    in
        all (\i -> checkIS (getFloorItems' state i) (False, False)) [0, 1, 2, 3]

-- given a state it returns all the valid states it can reach in one step
expandState :: State -> S.Seq State
expandState state = let
        -- items on the current floor
        allItems = getFloorItems state

        newItemsHelper :: S.Seq ItemSet -> Item -> S.Seq ItemSet
        newItemsHelper s i =
            i S.<| foldl (\ss ii -> (i .|. ii) S.<| ss) s
                (expandItemSet allItems (i `unsafeShiftL` 1))

        -- pairs of items it can move
        newItems :: S.Seq ItemSet
        newItems = foldl newItemsHelper S.empty $ expandItemSet allItems 1

        -- states when trying to move down
        sdown = if getFloor state == 0 then S.empty else
            S.filter checkState $ fmap (moveDown state) newItems

        -- states when trying to move up
        sup = if getFloor state == 3 then S.empty else
            S.filter checkState $ fmap (moveUp state) newItems
    in
        sup S.>< sdown

-- solves the problem using a BFS
solveBFS :: State -> Int
solveBFS state = let
        -- expands the state, filters out already visited states
        -- returns the number of moves needed to reach the final state
        step :: S.Seq (State, Int) -> HS.HashSet State -> Int
        step q found
            | stateIsFinal s = d
            | otherwise = let
                    states = S.filter (not . flip HS.member found) $ expandState s
                in
                    step
                        (qs S.>< fmap (\x -> (x, d + 1)) states)
                        (foldl (flip HS.insert) found states)
            where ((s, d), qs) = (S.index q 0, S.drop 1 q)
    in
        step (S.singleton (state, 0)) (HS.singleton state)

main :: IO ()
main = do
    contents <- readFile "input.txt"
    let floors1 = map parseFloor $ lines contents

    let (items1, _) = dataToNumeric . map snd . sortOn fst $ floors1

    let initialState1 = initState 0 $ map combineItems items1

    -- this should run fast
    print $ solveBFS initialState1

    let extra2 = [('G', "elerium"), ('M', "elerium"), ('G', "dilithium"), ('M', "dilithium")]

    -- add extra2 to the first floor
    let floors2 = (0, snd (head floors1) ++ extra2) : tail floors1

    let (items2, _) = dataToNumeric . map snd . sortOn fst $ floors2
    let initialState2 = initState 0 $ map combineItems items2

    -- this will take some time
    print $ solveBFS initialState2
