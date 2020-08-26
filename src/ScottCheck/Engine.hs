{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE FlexibleContexts #-}
module ScottCheck.Engine where

import ScottCheck.GameData
import ScottCheck.Utils

import Control.Monad.Reader
import Control.Monad.Writer
import Control.Monad.State
import Data.SBV.MTL ()

import Control.Lens
import Control.Monad
import Data.Char (toUpper)
import Data.Maybe
import Data.Int
import qualified Data.Map as M
import Data.Array as A
import Data.Bifunctor
import Data.Either (partitionEithers)

import GHC.Generics (Generic)
import Data.SBV
import Data.SBV.Maybe (sNothing, sJust)
import qualified Data.SBV.Maybe as SBV

type SInput = (SInt16, SInt16)

data S = S
    { _currentRoom :: SInt16
    , _savedRoom :: SInt16
    , _needLook :: SBool
    , _itemLocations :: Array Int16 SInt16
    , _endState :: SMaybe Bool
    } deriving (Show, Generic, Mergeable)
makeLenses ''S

type Engine = ReaderT Game (WriterT [SString] (State S))

-- -- XXX TODO
-- dirNames :: [String]
-- dirNames = ["North", "South", "East", "West", "Up", "Down"]

carried :: Int16
carried = 255

say :: SString -> Engine ()
say = tell . (:[])

say_ :: String -> Engine ()
say_ = say . literal

initState :: Game -> S
initState game = S
    { _currentRoom = literal $ gameStartRoom game
    , _savedRoom = literal 0
    , _needLook = sTrue
    , _itemLocations = fmap (\(Item _ _ _ loc) -> literal loc) $ gameItems game
    , _endState = sNothing
    }

runGame :: Game -> Engine a -> State S (a, [SString])
runGame game act = runWriterT $ runReaderT act game

stepWorld :: Engine (SMaybe Bool)
stepWorld = do
    performAuto
    look
    finished

stepPlayer :: SInput -> Engine (SMaybe Bool)
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

    itemLocs <- use itemLocations
    items <- asks gameItems
    let itemLocs' = [ (loc, desc) | (Item _ _ desc _, loc) <- zip (A.elems items) (A.elems itemLocs) ]
        anyHere = sAny ((.== here) . fst) itemLocs'
    sWhen anyHere $ do
        say_ "I can also see:"
        forM_ itemLocs' $ \(loc, desc) -> sWhen (loc .== here) $ say_ $ " * " <> desc

die :: Engine ()
die = endState .= sJust sFalse

finished :: Engine (SMaybe Bool)
finished = use endState

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

builtin :: SInput -> Engine ()
builtin (verb, noun) = sCase verb
    [ (1, builtin_go)
    , (10, builtin_get)
    , (18, builtin_drop)
    ]
    (return ())
  where
    builtin_go = ite (sNot $ 1 .<= noun .&& noun .<= 6) badDir $ do
        let dir = noun - 1
        here <- use currentRoom
        SRoom exits _ <- asks $ (.! here) . fmap sRoom . gameRooms
        let newRoom = select exits 0 dir
        ite (newRoom .== 0) blocked $ do
            currentRoom .= newRoom
      where
        badDir = say_ "I don't know how to go in that direction"
        blocked = say_ "I can't go in that direction."

    builtin_get = do
        locs <- use itemLocations
        here <- use currentRoom
        item <- parseItem
        ite (select (A.elems locs) (-1) item ./= here) (say_ "It's beyond my power to do that.") $ do
            move item (literal carried)
            say_ "OK."

    builtin_drop = do
        locs <- use itemLocations
        here <- use currentRoom
        item <- parseItem
        ite (select (A.elems locs) (-1) item ./= literal carried) (say_ "It's beyond my power to do that.") $ do
            move item here
            say_ "OK."

    parseItem = do
        items <- asks gameItems
        return $ SBV.fromMaybe (-1) $ sFindIndex (\(Item _ name _ _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items

type SCondition = (SInt16, SInt16)
type SInstr = SInt16

data SAction = SAction SInput [SCondition] [SInstr] deriving (Show, Generic, Mergeable)

sAction :: Action -> SAction
sAction (Action input conds instrs) = SAction (bimap literal literal input) (map (bimap literal literal) conds) (map literal instrs)

performAuto :: Engine ()
performAuto = do
    actions <- asks $ fmap sAction . gameActions
    forM_ actions $ \(SAction (verb', noun') conds instrs) ->
        sWhen (verb' .== 0) $ void $ execIf conds instrs

perform :: SInput -> Engine ()
perform input = do
    handled <- performMatching input
    sUnless handled $ void $ builtin input

performMatching :: SInput -> Engine SBool
performMatching (verb, noun) = do
    actions <- asks $ fmap sAction . gameActions
    loop actions
  where
    loop [] = return sFalse
    loop (action@(SAction (verb', noun') conds instrs):actions) = do
        ite (sNot $ match (verb', noun')) (loop actions) $ do
            b <- execIf conds instrs
            ite b (return sTrue) (loop actions)

    match (verb', noun') = verb' .== verb .&& (noun' .== 0 .|| noun' .== noun)

execIf :: [SCondition] -> [SInstr] -> Engine SBool
execIf conds instrs = do
    (bs, args) <- partitionEithers <$> mapM evalCond conds
    let b = sAnd bs
    sWhen b $ exec args instrs
    return b

evalCond :: SCondition -> Engine (Either SBool SInt16)
evalCond (op, dat) = ite (op .== 0) (return $ Right dat) $ fmap Left $
    sCase op
      [ (1, isItemAt (literal carried))
      , (2, isItemAt =<< use currentRoom)
      , (3, itemAvailable)
      , (4, uses currentRoom (.== dat))
      , (5, sNot <$> (isItemAt =<< use currentRoom))
      , (6, sNot <$> isItemAt (literal carried))
      , (7, sNot <$> uses currentRoom (.== dat))
      , (10, someCarried)
      , (11, sNot <$> someCarried)
      , (12, sNot <$> itemAvailable)
      , (13, sNot <$> isItemAt (literal 0))
      , (14, isItemAt (literal 0))
      , (17, itemHasMoved)
      , (18, sNot <$> itemHasMoved)
      ]
      (return sTrue)
  where
    isItemAt room = do
        loc <- uses itemLocations (.! dat)
        return $ loc .== room

    itemAvailable = do
        loc <- uses itemLocations (.! dat)
        here <- use currentRoom
        return $ loc .== literal carried .|| loc .== here

    someCarried = do
        locs <- use itemLocations
        return $ sAny (.== literal carried) $ A.elems locs

    itemHasMoved = do
        loc <- uses itemLocations (.! dat)
        locs0 <- asks gameItems
        let loc0 = fmap (\(Item _ _ _ loc) -> literal loc) locs0 .! dat
        return $ loc ./= loc0

type Exec = StateT [SInt16] Engine

nextArg :: Exec SInt16
nextArg = do
    xs <- get
    case xs of
        (x:xs) -> do
            put xs
            return x
        [] -> error "Out of args"

exec :: [SInt16] -> [SInstr] -> Engine ()
exec args instrs = flip evalStateT args $ mapM_ execInstr instrs

execInstr :: SInstr -> Exec ()
execInstr instr =
    ite (1 .<= instr .&& instr .<= 51) (msg instr) $
    ite (102 .<= instr) (msg $ instr - 50) $
    sCase instr
        [ (0, return ())
        , (53, bringHere)
        , (54, teleport)
        , (55, destroy)
        , (59, destroy)
        , (62, moveTo)
        , (63, gameOver)
        , (64, lookAround)
        , (65, showScore)
        , (66, showInventory)
        , (72, swapItems)
        , (74, takeItem)
        , (75, moveNextTo)
        , (76, lookAround)
        , (80, swapRoom)
        , (88, return ()) -- Sleep 2s...
        ]
        (return ())
  where
    msg i = lift $ do
        s <- asks $ (.! i) . fmap literal . gameMessages
        say s

    bringHere = do
        item <- nextArg
        here <- lift $ use currentRoom
        lift $ move item here

    moveTo = do
        item <- nextArg
        room <- nextArg
        lift $ move item room

    takeItem = do
        item <- nextArg
        lift $ move item (literal carried)

    teleport = do
        room <- nextArg
        lift $ currentRoom .= room

    destroy = do
        item <- nextArg
        lift $ move item 0

    gameOver = lift die

    showScore = lift $ do
        treasury <- asks gameTreasury
        items <- asks gameItems
        itemLocations <- use itemLocations
        let treasureLocs = [ loc | (loc, Item True _ _ _) <- zip (A.elems itemLocations) (A.elems items) ]
            score = count (.== literal treasury) treasureLocs
        maxScore <- asks gameMaxScore

        say_ $ "Your score is " <> show score <> " out of a possible " <> show maxScore <> "."
        sWhen (score .== literal maxScore) $ endState .= sJust sTrue

    showInventory = lift $ say_ "INVENTORY"

    lookAround = lift look

    swapItems = do
        item1 <- nextArg
        item2 <- nextArg
        lift $ do
            loc1 <- uses itemLocations (.! item1)
            loc2 <- uses itemLocations (.! item2)
            move item1 loc2
            move item2 loc1

    swapRoom = lift $ do
        here <- use currentRoom
        there <- use savedRoom
        savedRoom .= here
        currentRoom .= there

    moveNextTo = do
        item <- nextArg
        target <- nextArg
        lift $ do
            loc <- uses itemLocations (.! target)
            move item loc

move :: SInt16 -> SInt16 -> Engine ()
move item loc = itemLocations %= replaceAt item loc
