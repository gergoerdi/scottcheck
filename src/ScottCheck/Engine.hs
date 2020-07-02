{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ScottCheck.Engine where
    -- ( MonadInteractive(..)
    -- , runGame
    -- ) where

import ScottCheck.GameData
import ScottCheck.Utils

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.SBV.MTL ()
-- import Control.Monad.Except

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

import GHC.Generics (Generic)
import Data.SBV
import Data.SBV.Maybe (sNothing, sJust)
import qualified Data.SBV.Maybe as SBV
-- import Data.SBV.String ((.++))
-- import Data.SBV.Trans

-- class (Monad m) => MonadInteractive m where
--     emit :: String -> m ()
--     input :: m (String, String)

-- instance MonadInteractive IO where
--     emit = putStrLn
--     input = do
--         putStr "> "
--         line <- getLine
--         return $ case words line of
--             (w1:w2:_) -> (w1, w2)
--             [w1] -> (w1, "")
--             [] -> ("", "")

data S = S
    { _currentRoom :: SInt16
    , _needLook :: SBool
    , _itemLocations :: Array Int16 SInt16
    , _dead :: SBool
    } deriving (Show, Generic, Mergeable)
makeLenses ''S

-- type BaseM = SymbolicT (WriterT [SString] IO)
type BaseM = Writer [SString]
type Engine = ReaderT Game (StateT S BaseM)

dirNames :: [String]
dirNames = ["North", "South", "East", "West", "Up", "Down"]

carried :: Int16
carried = 255

say :: SString -> Engine ()
say = lift . lift . tell . (:[])

say_ :: String -> Engine ()
say_ = say . literal

runGame :: Game -> Engine a -> BaseM a
runGame game act = evalStateT (runReaderT act game) s0
  where
    s0 = S
        { _currentRoom = literal $ gameStartRoom game
        , _needLook = sTrue
        , _itemLocations = fmap (\(Item _ _ _ loc) -> literal loc) $ gameItems game
        , _dead = sFalse
        }

stepWorld :: Engine (SMaybe Bool)
stepWorld = do
    perform (0, 0)
    look
    finished

stepPlayer :: (SInt16, SInt16) -> Engine (SMaybe Bool)
stepPlayer (v, n) = do
    perform (v, n)
    finished

data SRoom = SRoom [SInt16] SString deriving (Show, Generic, Mergeable)

sRoom :: Room -> SRoom
sRoom (Room exits desc) = SRoom (map literal exits) (literal desc)

look :: Engine ()
look = do
    here <- use currentRoom
    SRoom exits desc <- asks $ (.! here) . fmap sRoom . gameRooms
    say desc
    -- let availableExits = [ dir | (dir, nextRoom) <- zip dirNames exits, nextRoom ./= 0 ]
    -- unless (null availableExits) $ do
    --     say_ $ "Obvious exits: " <> intercalate ", " availableExits
    --     say_ ""

    itemLocs <- use itemLocations
    items <- asks gameItems
    let itemLocs' = [ (loc, desc) | (Item _ _ desc _, loc) <- zip (A.elems items) (A.elems itemLocs) ]
        anyHere = sAny ((.== here) . fst) itemLocs'
    sWhen anyHere $ do
        say_ "I can also see:"
        forM_ itemLocs' $ \(loc, desc) -> sWhen (loc .== here) $ say $ literal $ " * " <> desc

score :: Engine SInt16
score = do
    treasury <- asks gameTreasury
    items <- asks gameItems
    itemLocations <- use itemLocations
    let treasureLocs = [ loc | (loc, Item True _ _ _) <- zip (A.elems itemLocations) (A.elems items) ]
    return $ count (.== literal treasury) treasureLocs

finished :: Engine (SMaybe Bool)
finished = do
    dead <- use dead
    score <- score
    maxScore <- asks gameMaxScore
    return $ ite dead (sJust sFalse) $
      ite (score .== literal maxScore) (sJust sTrue) $
      sNothing

parseInput :: Game -> String -> String -> Maybe (Int16, Int16)
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


-- askInput :: (MonadInteractive m) => Engine m (Int16, Int16)
-- askInput = do
--     wordLen <- asks gameWordLength
--     verbs <- asks gameVerbs
--     nouns <- asks gameNouns
--     let parse dict s = case s of
--             "" -> Just (-1)
--             s -> M.lookup s' dict
--           where
--             s' = take (fromIntegral wordLen) . map toUpper $ s

--     (w1, w2) <- lift . lift $ input

--     let verb = parse verbs w1
--         noun = parse nouns w2
--     case (verb, noun) of
--         (Just (-1), Just (-1)) -> askInput
--         (Nothing, Just (-1)) | Just dir <- parse nouns w1, 1 <= dir && dir <= 6 -> return (1, dir)
--         (Just verb, Just noun) -> return (verb, noun)
--         _ -> do
--             say "Unknown words"
--             askInput


    -- runGame :: (MonadInteractive m) => Game -> m Bool
-- runGame game = flip evalStateT s0 $ flip runReaderT game $ untilJust $ do
--     gameTurn
--     finished
--   where
--     s0 = S
--         { _currentRoom = gameStartRoom game
--         , _needLook = True
--         , _itemLocations = fmap (\(Item _ _ _ loc) -> loc) $ gameItems game
--         , _dead = False
--         }

--     gameTurn = do
--         look
--         perform (0, 0)
--         look
--         done <- isJust <$> finished
--         unless done $ do
--             (verb, noun) <- askInput
--             handled <- perform (verb, noun)
--             unless handled $ say "I don't understand."

builtin :: (SInt16, SInt16) -> Engine SBool
builtin (verb, noun) = sCase verb (return sFalse)
    [ (1, builtin_go)
    , (10, builtin_get)
    , (18, builtin_drop)
    ]
  where
    builtin_go = ite (sNot $ 1 .<= noun .&& noun .<= 6) badDir $ do
        let dir = noun - 1
        here <- use currentRoom
        SRoom exits _ <- asks $ (.! here) . fmap sRoom . gameRooms
        let newRoom = select exits 0 dir
        ite (newRoom .== 0) blocked $ do
            currentRoom .= newRoom
        return sTrue
      where
        badDir = do
            say_ "I don't know how to go in that direction"
            return sTrue

        blocked = say_ "I can't go in that direction."

    builtin_get = do
        locs <- use itemLocations
        here <- use currentRoom
        items <- asks gameItems
        let item = SBV.fromJust $ sFindIndex (\(Item _ name _ _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items
        ite (select (A.elems locs) (-1) item ./= here) (say_ "It's beyond my power to do that.") $ do
            move item (literal carried)
            say_ "OK."
        return sTrue

    builtin_drop = do
        locs <- use itemLocations
        here <- use currentRoom
        items <- asks gameItems
        let item = SBV.fromJust $ sFindIndex (\(Item _ name _ _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items
        ite (select (A.elems locs) (-1) item ./= literal carried) (say_ "It's beyond my power to do that.") $ do
            move item here
            say_ "OK."
        return sTrue

perform :: (SInt16, SInt16) -> Engine ()
perform (verb, noun) = do
    handled <- performMatching (verb, noun)
    sUnless handled $ void $ builtin (verb, noun)
    return ()

performMatching :: (SInt16, SInt16) -> Engine SBool
performMatching (verb, noun) = return sFalse

-- performMatching (verb, noun) = do
--     actions <- asks gameActions
--     flip evalStateT actions $ loop False
--   where
--     loop handled = do
--         action <- getNext
--         -- traceShow (verb, noun, action) $ return ()
--         case action of
--             Nothing -> return handled
--             Just (Action (verb', noun') conds instrs)
--               | verb' == verb, verb' == 0 || (noun' `elem` [0, noun]) -> do
--                   result <- lift $ do
--                       (bs, args) <- unzip <$> mapM evalCond conds
--                       args <- return $ catMaybes args
--                       if and bs then {- traceShow action $ -} flip evalStateT args $ evalInstrs instrs else return NoMatch
--                   case result of
--                       Done | verb /= 0 -> return True
--                            | otherwise -> loop True
--                       Continue -> loop True
--                       NoMatch -> loop handled
--             _ -> loop handled

-- evalCond :: (MonadInteractive m) => Condition -> Engine m (Bool, Maybe Int16)
-- evalCond (0, dat) = return (True, Just dat)
-- -- evalCond (op, dat) = fmap (, Nothing) $ case op of
-- --     1 -> isItemAt dat carried
-- --     2 -> isItemAt dat =<< use currentRoom
-- --     4 -> uses currentRoom (== dat)
-- --     6 -> not <$> isItemAt dat carried
-- --     12 -> do
-- --         loc <- uses itemLocations (! dat)
-- --         here <- use currentRoom
-- --         return $ loc `notElem` [carried, here]
-- --   where
-- --     isItemAt item room = do
-- --         loc <- uses itemLocations (! item)
-- --         return $ loc == room

-- data InstrResult
--     = Done
--     | Continue
--     | NoMatch
--     deriving (Show)

-- type Exec m = StateT [Int16] (Engine m)

-- nextArg :: (MonadInteractive m) => Exec m Int16
-- nextArg = fromJust <$> getNext

-- getNext :: (MonadState [a] m) => m (Maybe a)
-- getNext = do
--     xs <- get
--     case xs of
--         (x:xs) -> do
--             put xs
--             return $ Just x
--         [] -> return Nothing

-- evalInstrs :: (MonadInteractive m) => [Instr] -> Exec m InstrResult
-- -- evalInstrs [] =
-- evalInstrs instrs = do
--     mapM_ evalInstr instrs
--     return Done -- TODO: return Continue if any instr returns Continue

-- evalInstr :: (MonadInteractive m) => Instr -> Exec m InstrResult
-- -- evalInstr instr
-- --   | 1 <= instr && instr <= 51 = msg instr >> return Done
-- --   | 102 <= instr = msg (instr - 50) >> return Done
-- evalInstr 0 = return Done
-- -- evalInstr 54 = do
-- --     room <- nextArg
-- --     lift $ currentRoom .= room
-- --     lift $ needLook .= True
-- --     return Done
-- -- evalInstr 63 = lift die >> return Done
-- -- evalInstr 64 = lift lookNow >> return Done
-- -- evalInstr 65 = lift $ do
-- --     score <- score
-- --     numTreasures <- asks gameMaxScore
-- --     let percentage = 100 * fromIntegral score / fromIntegral numTreasures :: Double
-- --     say $ printf "I've stored %d treasures." score
-- --     say $ printf "On a scale of 0 to 100, that rates %.0f." percentage
-- --     return Done
-- -- evalInstr 66 = lift (say "TODO: INVENTORY") >> return Done
-- -- evalInstr 72 = do
-- --     item1 <- nextArg
-- --     item2 <- nextArg
-- --     lift $ do
-- --         here <- use currentRoom
-- --         loc1 <- uses itemLocations (! item1)
-- --         loc2 <- uses itemLocations (! item2)
-- --         when (loc1 == here || loc2 == here) $ needLook .= True
-- --         move item1 loc2
-- --         move item2 loc1
-- --     return Done
-- evalInstr instr = error $ unwords ["evalInstr", show instr]


-- msg :: (MonadInteractive m) => Int16 -> Exec m ()
-- msg i = lift $ do
--     s <- asks $ (! i) . gameMessages
--     say s

-- die :: (MonadInteractive m) => Engine m ()
-- die = do
--     say "The game is now over"
--     dead .= sTrue
--     return () -- TODO

move :: SInt16 -> SInt16 -> Engine ()
move item loc = itemLocations %= replaceAt item loc
