{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
module ScottCheck.Engine where

import ScottCheck.GameData
import ScottCheck.Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State

import Control.Lens
import Control.Monad
import Data.List (findIndex)
import Data.Char (toUpper)
import Data.Maybe
import Data.Int
import qualified Data.Map as M
import Data.Array as A
import Data.Either (partitionEithers)

data S = S
    { _currentRoom :: Int16
    , _needLook :: Bool
    , _itemLocations :: Array Int16 Int16
    , _dead :: Bool
    } deriving Show
makeLenses ''S

type Engine = ReaderT Game (WriterT [String] (State S))

-- -- XXX TODO
-- dirNames :: [String]
-- dirNames = ["North", "South", "East", "West", "Up", "Down"]

carried :: Int16
carried = 255

say :: String -> Engine ()
say = tell . (:[])

initState :: Game -> S
initState game = S
    { _currentRoom = gameStartRoom game
    , _needLook = True
    , _itemLocations = fmap (\(Item _ _ _ loc) -> loc) $ gameItems game
    , _dead = False
    }

runGame :: Game -> Engine a -> State S (a, [String])
runGame game act = runWriterT $ runReaderT act game

stepWorld :: Engine (Maybe Bool)
stepWorld = do
    performAuto
    look
    finished

stepPlayer :: Input -> Engine (Maybe Bool)
stepPlayer (v, n) = do
    perform (v, n)
    finished

look :: Engine ()
look = do
    here <- use currentRoom
    Room exits desc <- asks $ (! here) . gameRooms
    say desc

    itemLocs <- use itemLocations
    items <- asks gameItems
    let itemsHere = [ desc | (Item _ _ desc _, loc) <- zip (A.elems items) (A.elems itemLocs), loc == here ]
    unless (null itemsHere) $ do
        say "I can also see:"
        mapM_ (\desc -> say $ " * " <> desc) itemsHere

score :: Engine Int16
score = do
    treasury <- asks gameTreasury
    items <- asks gameItems
    itemLocations <- use itemLocations
    let treasureLocs = [ loc | (loc, Item True _ _ _) <- zip (A.elems itemLocations) (A.elems items) ]
    return $ count (== treasury) treasureLocs

die :: Engine ()
die = dead .= True

finished :: Engine (Maybe Bool)
finished = do
    dead <- use dead

    maxScore <- asks gameMaxScore
    score <- score
    let haveAllTreasure = score == maxScore

    return $
      if dead then Just False else
      if haveAllTreasure then Just True else
      Nothing

parseInput :: Game -> String -> String -> Maybe Input
parseInput Game{..} w1 w2 = case (verb, noun) of
    (Nothing, Just (-1)) | Just dir <- parse gameNouns w1, 1 <= dir && dir <= 6 -> Just (1, dir)
    (Just verb, Just noun) -> Just (verb, noun)
    _ -> Nothing
  where
    verb = parse gameVerbs w1
    noun = parse gameNouns w2

    parse dict "" = Just (-1)
    parse dict s = M.lookup (normalize s) dict

    normalize = map toUpper . take (fromIntegral gameWordLength)

builtin :: Input -> Engine ()
builtin (verb, noun) = case verb of
    1 -> builtin_go
    10 -> builtin_get
    18 -> builtin_drop
    _ -> return ()
  where
    builtin_go = if not (1 <= noun && noun <= 6) then badDir else do
        let dir = noun - 1
        here <- use currentRoom
        Room exits _ <- asks $ (! here) . gameRooms
        let newRoom = exits !! fromIntegral dir
        if newRoom == 0 then blocked else do
            currentRoom .= newRoom
      where
        badDir = say "I don't know how to go in that direction"
        blocked = say "I can't go in that direction."

    builtin_get = do
        locs <- use itemLocations
        here <- use currentRoom
        items <- asks gameItems
        let item = fmap fromIntegral $
                findIndex (\(Item _ name _ _) -> maybe False (noun ==) name) $ A.elems items
        case item of
            Just item | locs ! item == here -> do
                move item carried
                say "OK."
            _ -> do
                say "It's beyond my power to do that."

    builtin_drop = do
        locs <- use itemLocations
        here <- use currentRoom
        items <- asks gameItems
        let item = fmap fromIntegral $
                findIndex (\(Item _ name _ _) -> maybe False (noun ==) name) $ A.elems items
        case item of
            Just item | locs ! item == carried -> do
                move item here
                say "OK."
            _ -> do
                say "It's beyond my power to do that."

performAuto :: Engine ()
performAuto = do
    actions <- asks gameActions
    forM_ actions $ \(Action (verb', noun') conds instrs) ->
        when (verb' == 0) $ void $ execIf conds instrs

perform :: Input -> Engine ()
perform input = do
    handled <- performMatching input
    unless handled $ builtin input

performMatching :: Input -> Engine Bool
performMatching (verb, noun) = do
    actions <- asks gameActions
    loop actions
  where
    loop [] = return False
    loop (action@(Action pattern conds instrs):actions)
      | match pattern = execIf conds instrs
      | otherwise = loop actions

    match (verb', noun') = verb' == verb && noun' `elem` [0, noun]

execIf :: [Condition] -> [Instr] -> Engine Bool
execIf conds instrs = do
    (bs, args) <- partitionEithers <$> mapM evalCond conds
    let b = and bs
    when b $ exec args instrs
    return b

evalCond :: Condition -> Engine (Either Bool Int16)
evalCond (0, dat) = return $ Right dat
evalCond (op, dat) = fmap Left $ case op of
    1 -> isItemAt carried
    2 -> isItemAt =<< use currentRoom
    4 -> uses currentRoom (== dat)
    6 -> not <$> isItemAt carried
    12 -> not <$> itemAvailable
    _ -> return True
  where
    isItemAt room = do
        loc <- uses itemLocations (! dat)
        return $ loc == room

    itemAvailable = do
        loc <- uses itemLocations (! dat)
        here <- use currentRoom
        return $ loc `elem` [carried, here]

type Exec = StateT [Int16] Engine

nextArg :: Exec Int16
nextArg = do
    xs <- get
    case xs of
        (x:xs) -> do
            put xs
            return x
        [] -> error "Out of args"

exec :: [Int16] -> [Instr] -> Engine ()
exec args instrs = flip evalStateT args $ mapM_ execInstr instrs

execInstr :: Instr -> Exec ()
execInstr instr = case instr of
    _ | 1 <= instr && instr <= 51 -> msg instr
    _ | 102 <= instr -> msg $ instr - 50
    0 -> return ()
    54 -> teleport
    63 -> gameOver
    64 -> lookAround
    65 -> showScore
    66 -> showInventory
    72 -> swapItems
    _ -> return ()
  where
    msg i = lift $ do
        s <- asks $ (! i) . gameMessages
        say s

    teleport = do
        room <- nextArg
        lift $ currentRoom .= room

    gameOver = lift die

    showScore = lift $ say "SCORE"
    showInventory = lift $ say "INVENTORY"
    lookAround = lift look

    swapItems = do
        item1 <- nextArg
        item2 <- nextArg
        lift $ do
            loc1 <- uses itemLocations (! item1)
            loc2 <- uses itemLocations (! item2)
            move item1 loc2
            move item2 loc1

move :: Int16 -> Int16 -> Engine ()
move item loc = itemLocations %= replaceAt item loc
