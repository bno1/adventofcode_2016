import qualified Data.IntMap.Strict as M
import Control.Monad.State
import Data.Maybe
import Debug.Trace

-- lower value of the bot we need for answer 1
checkLow :: Int
checkLow = 17

-- higher value of the bot we need for answer 1
checkHigh :: Int
checkHigh = 61

-- bot command
data Command =
    None |
    GiveBot Int |
    Output Int
    deriving (Show)

-- bot description
-- (Command, Command) represent the commands for the lower and higer values
data Bot =
    -- bot with no values
    BotEmpty {botCmd :: (Command, Command)} |
    -- bot with only one value
    BotSingle {botCmd :: (Command, Command), botLow :: Int} |
    -- bot with both values
    BotFull {botCmd :: (Command, Command), botLow :: Int, botHigh :: Int}
    deriving (Show)

-- parsed instruction
data Instruction =
    GiveValue {instrBot :: Int, instrVal :: Int} |
    SetCommand {instrBot :: Int, instrCmd :: (Command, Command)}
    deriving (Show)

-- simulation data (bot descriptions and ouputs)
data SimulationData = SimulationData {
    simDataBots :: M.IntMap Bot,
    simDataOutputs :: M.IntMap Int
}

-- create empty bot with no commands
initBot :: Bot
initBot = BotEmpty (None, None)

-- give a value to a bot
-- cannot give value to a bot with 2 values
give :: Int -> Bot -> Bot
give v (BotEmpty c) = BotSingle c v
give v (BotSingle c bv) = BotFull c (min bv v) (max bv v)
give _ BotFull{} = error "Cannot give value to full bot"

-- removes the values from a bot
clearBot :: Bot -> Bot
clearBot (BotFull cmd _ _) = BotEmpty cmd
clearBot _ = error "Cannot clear non-full bot"

-- parses an instruction from the input file
parseInstruction :: String -> Maybe Instruction
parseInstruction s = case words s of
    ["value", vstr, "goes", "to", "bot", bstr] ->
        Just $ GiveValue b v
        where b = read bstr
              v = read vstr
    ["bot", bstr, "gives", "low", "to", dst1, id1str, "and", "high", "to", dst2, id2str] ->
        Just $ SetCommand b (constr1 id1, constr2 id2)
        where constr1 = if dst1 == "bot" then GiveBot else Output
              constr2 = if dst2 == "bot" then GiveBot else Output
              id1 = read id1str
              id2 = read id2str
              b = read bstr
    _ -> Nothing

-- turns the instruction list into a map that describes each bot with it's
-- command and starting values
setupBots :: [Instruction] -> M.IntMap Bot
setupBots = let
        -- used for alter
        func :: Instruction -> Maybe Bot -> Bot
        func (GiveValue _ v) Nothing = give v initBot
        func (GiveValue _ v) (Just bot) = give v bot
        func (SetCommand _ c) Nothing = initBot{botCmd = c}
        func (SetCommand _ c) (Just bot) = bot{botCmd = c}
    in foldl (\m instr -> M.alter (Just . func instr) (instrBot instr) m) M.empty

-- sets the value of an output
setOutput :: Int -> Int -> SimulationData -> SimulationData
setOutput o v sd = sd{simDataOutputs = M.insert o v $ simDataOutputs sd}

-- applies a modifier to a bot
modifyBot :: (Bot -> Bot) -> Int -> SimulationData -> SimulationData
modifyBot f b sd = sd{simDataBots = M.adjust f b $ simDataBots sd}

-- applies the command on the specified bot
applyCommand :: Command -> Int -> SimulationData -> SimulationData
applyCommand None _ = id
applyCommand (GiveBot b) v = modifyBot (give v) b
applyCommand (Output o) v = setOutput o v

-- given the starting bot description map it simulates each bot recursively
simulate :: M.IntMap Bot -> SimulationData
simulate m = let
        step :: Int -> State SimulationData ()
        step b = do
            (SimulationData bots _) <- get

            case M.lookup b bots of
                -- bot has 2 values and can execute its command
                Just (BotFull (c1, c2) l h) -> do
                    -- check if it is the bot for answer 1
                    when (l == checkLow && h == checkHigh) $
                        traceShow b (return ())

                    -- clear bot and give values to other bots or outputs
                    modify $
                        applyCommand c2 h .
                        applyCommand c1 l .
                        modifyBot clearBot b

                    -- recurse if it gives the lower value to a bot
                    case c1 of
                        GiveBot bi -> step bi
                        _ -> return ()

                    -- recurse if it gives the higher value to a bot
                    case c2 of
                        GiveBot bi -> step bi
                        _ -> return ()
                _ -> return ()
    in
        -- for every bot try to simulate it
        M.foldlWithKey
            (\sd k _ -> execState (step k) sd)
            (SimulationData m M.empty)
            m

main :: IO ()
main = do
    contents <- readFile "input.txt"

    let instructions = mapMaybe parseInstruction $ lines contents
    let bots = setupBots instructions

    let outputs = simDataOutputs $ simulate bots
    print . product $ mapMaybe (`M.lookup` outputs) [0, 1, 2]
