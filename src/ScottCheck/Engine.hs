{-# LANGUAGE RecordWildCards, TemplateHaskell, ViewPatterns, FlexibleContexts, TupleSections #-}
module ScottCheck.Engine
    ( MonadInteractive(..)
    , runGame
    ) where

import ScottCheck.GameData

import Control.Monad.State
import Control.Monad.Reader
import Control.Lens
import Control.Monad
import Control.Monad.Loops
import Data.List (elemIndex, intercalate)
import Data.Char (toUpper)
import Data.Maybe
import Debug.Trace
import Data.Int
import Text.Printf
import qualified Data.Map as M
import Data.Array as A

class (Monad m) => MonadInteractive m where
    emit :: String -> m ()
    input :: m (String, String)

instance MonadInteractive IO where
    emit = putStrLn
    input = do
        putStr "> "
        line <- getLine
        return $ case words line of
            (w1:w2:_) -> (w1, w2)
            [w1] -> (w1, "")
            [] -> ("", "")

data S = S
    { _currentRoom :: Int16
    , _needLook :: Bool
    , _itemLocations :: Array Int16 Int16
    , _dead :: Bool
    } deriving (Show)
makeLenses ''S

type Engine m = ReaderT Game (StateT S m)

dirNames :: [String]
dirNames = ["North", "South", "East", "West", "Up", "Down"]

carried :: Int16
carried = 255

verb_go = 1
verb_get = 10
verb_drop = 18

say :: (MonadInteractive m) => String -> Engine m ()
say = lift . lift . emit

finished :: (MonadInteractive m) => Engine m (Maybe Bool)
finished = do
    dead <- use dead
    score <- score
    maxScore <- asks gameMaxScore
    return $ if dead then Just False else if score == maxScore then Just True else Nothing

runGame :: (MonadInteractive m) => Game -> m Bool
runGame game = flip evalStateT s0 $ flip runReaderT game $ untilJust $ do
    gameTurn
    finished
  where
    s0 = S
        { _currentRoom = gameStartRoom game
        , _needLook = True
        , _itemLocations = fmap (\(Item _ _ _ loc) -> loc) $ gameItems game
        , _dead = False
        }

    gameTurn = do
        look
        perform (0, 0)
        look
        done <- isJust <$> finished
        unless done $ do
            (verb, noun) <- askInput
            handled <- perform (verb, noun)
            unless handled $ say "I don't understand."

builtin :: (MonadInteractive m) => (Int16, Int16) -> Engine m Bool
builtin (verb, noun)
    | verb == verb_go = builtin_go noun
    | verb == verb_get = builtin_get noun
    | verb == verb_drop = builtin_drop noun
    | otherwise = return False
  where
    builtin_go noun = case noun of
        (-1) -> do
            say "Go in which direction?"
            return True
        (subtract 1 -> dir) | 0 <= dir && dir <= 5 -> do
            rooms <- asks gameRooms
            Room exits _ <- uses currentRoom (rooms !)
            let newRoom = exits !! fromIntegral dir

            if newRoom /= 0 then do
                currentRoom .= newRoom
                lookNow
              else do
                say "I can't go in that direction."
            return True
        _ -> return False

    builtin_get noun = do
        locs <- use itemLocations
        here <- use currentRoom
        items <- asks gameItems
        let candidates = [i | (i, Item _ (Just name) _ _) <- A.assocs items, name == noun]
        case candidates of
            [item] | locs!item == here -> do
                -- TODO: check carry limit
                move item 255
                say "OK."
            _ -> do
                say "It's beyond my power to do that."
        return True

    builtin_drop noun = do
        locs <- use itemLocations
        here <- use currentRoom
        items <- asks gameItems
        let candidates = [i | (i, Item _ (Just name) _ _) <- A.assocs items, name == noun]
        case candidates of
            [item] | locs!item == 255 -> do
                -- TODO: check carry limit
                move item here
                say "OK."
            _ -> do
                say "It's beyond my power to do that."
        return True

perform :: (MonadInteractive m) => (Int16, Int16) -> Engine m Bool
perform (verb, noun) = do
    done <- performMatching (verb, noun)
    if done then return True else builtin (verb, noun)

performMatching :: (MonadInteractive m) => (Int16, Int16) -> Engine m Bool
performMatching (verb, noun) = do
    actions <- asks gameActions
    flip evalStateT actions $ loop False
  where
    loop handled = do
        action <- getNext
        -- traceShow (verb, noun, action) $ return ()
        case action of
            Nothing -> return handled
            Just (Action (verb', noun') conds instrs)
              | verb' == verb, verb' == 0 || (noun' `elem` [0, noun]) -> do
                  result <- lift $ do
                      (bs, args) <- unzip <$> mapM evalCond conds
                      args <- return $ catMaybes args
                      if and bs then {- traceShow action $ -} flip evalStateT args $ evalInstrs instrs else return NoMatch
                  case result of
                      Done | verb /= 0 -> return True
                           | otherwise -> loop True
                      Continue -> loop True
                      NoMatch -> loop handled
            _ -> loop handled

evalCond :: (MonadInteractive m) => Condition -> Engine m (Bool, Maybe Int16)
evalCond (0, dat) = return (True, Just dat)
-- evalCond (op, dat) = fmap (, Nothing) $ case op of
--     1 -> isItemAt dat 255
--     2 -> isItemAt dat =<< use currentRoom
--     4 -> uses currentRoom (== dat)
--     6 -> not <$> isItemAt dat 255
--     12 -> do
--         loc <- uses itemLocations (! dat)
--         here <- use currentRoom
--         return $ loc `notElem` [255, here]
--   where
--     isItemAt item room = do
--         loc <- uses itemLocations (! item)
--         return $ loc == room

data InstrResult
    = Done
    | Continue
    | NoMatch
    deriving (Show)

type Exec m = StateT [Int16] (Engine m)

nextArg :: (MonadInteractive m) => Exec m Int16
nextArg = fromJust <$> getNext

getNext :: (MonadState [a] m) => m (Maybe a)
getNext = do
    xs <- get
    case xs of
        (x:xs) -> do
            put xs
            return $ Just x
        [] -> return Nothing

evalInstrs :: (MonadInteractive m) => [Instr] -> Exec m InstrResult
-- evalInstrs [] =
evalInstrs instrs = do
    mapM_ evalInstr instrs
    return Done -- TODO: return Continue if any instr returns Continue

score :: (MonadInteractive m) => Engine m Int16
score = do
    treasury <- asks gameTreasury
    items <- asks gameItems
    itemLocations <- use itemLocations
    return $ sum [ 1 | (loc, Item True _ _ _) <- zip (A.elems itemLocations) (A.elems items), loc == treasury]

evalInstr :: (MonadInteractive m) => Instr -> Exec m InstrResult
-- evalInstr instr
--   | 1 <= instr && instr <= 51 = msg instr >> return Done
--   | 102 <= instr = msg (instr - 50) >> return Done
evalInstr 0 = return Done
-- evalInstr 54 = do
--     room <- nextArg
--     lift $ currentRoom .= room
--     lift $ needLook .= True
--     return Done
-- evalInstr 63 = lift die >> return Done
-- evalInstr 64 = lift lookNow >> return Done
-- evalInstr 65 = lift $ do
--     score <- score
--     numTreasures <- asks gameMaxScore
--     let percentage = 100 * fromIntegral score / fromIntegral numTreasures :: Double
--     say $ printf "I've stored %d treasures." score
--     say $ printf "On a scale of 0 to 100, that rates %.0f." percentage
--     return Done
-- evalInstr 66 = lift (say "TODO: INVENTORY") >> return Done
-- evalInstr 72 = do
--     item1 <- nextArg
--     item2 <- nextArg
--     lift $ do
--         here <- use currentRoom
--         loc1 <- uses itemLocations (! item1)
--         loc2 <- uses itemLocations (! item2)
--         when (loc1 == here || loc2 == here) $ needLook .= True
--         move item1 loc2
--         move item2 loc1
--     return Done
evalInstr instr = error $ unwords ["evalInstr", show instr]


msg :: (MonadInteractive m) => Int16 -> Exec m ()
msg i = lift $ do
    s <- asks $ (! i) . gameMessages
    say s
look :: (MonadInteractive m) => Engine m ()
look = do
    needed <- use needLook
    when needed lookNow
    needLook .= False

lookNow :: (MonadInteractive m) => Engine m ()
lookNow = do
    lookRoom
    lookItems
  where
    lookRoom = do
        here <- use currentRoom
        Room exits desc <- asks $ (! here) . gameRooms
        let availableExits = [ dir | (dir, nextRoom) <- zip dirNames exits, nextRoom /= 0 ]
        say desc
        unless (null availableExits) $ do
            say $ "Obvious exits: " <> intercalate ", " availableExits
            say ""

    lookItems = do
        here <- use currentRoom
        itemLocations <- use itemLocations
        items <- asks gameItems
        let itemsHere = [ desc | (Item _ _ desc _, loc) <- zip (A.elems items) (A.elems itemLocations), loc == here]
        unless (null itemsHere) $ do
            say "I can also see:"
            forM_ itemsHere $ \s -> do
                say $ " * " <> s

askInput :: (MonadInteractive m) => Engine m (Int16, Int16)
askInput = do
    wordLen <- asks gameWordLength
    verbs <- asks gameVerbs
    nouns <- asks gameNouns
    let parse dict s = case s of
            "" -> Just (-1)
            s -> M.lookup s' dict
          where
            s' = take (fromIntegral wordLen) . map toUpper $ s

    (w1, w2) <- lift . lift $ input

    let verb = parse verbs w1
        noun = parse nouns w2
    case (verb, noun) of
        (Just (-1), Just (-1)) -> askInput
        (Nothing, Just (-1)) | Just dir <- parse nouns w1, 1 <= dir && dir <= 6 -> return (verb_go, dir)
        (Just verb, Just noun) -> return (verb, noun)
        _ -> do
            say "Unknown words"
            askInput

die :: (MonadInteractive m) => Engine m ()
die = do
    say "The game is now over"
    dead .= True
    return () -- TODO

move :: (MonadInteractive m) => Int16 -> Int16 -> Engine m ()
move item loc = itemLocations %= (// [(item, loc)])
