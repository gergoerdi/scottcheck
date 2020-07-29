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
import Control.Monad.Loops
import Data.List (elemIndex, intercalate)
import Data.Char (toUpper)
import Data.Maybe
import Data.Int
import Text.Printf
import qualified Data.Map as M
import Data.Array as A
import Data.Bifunctor
import Data.Either (partitionEithers)

import Debug.Trace

import GHC.Generics (Generic)
import Data.SBV
import Data.SBV.Maybe (sNothing, sJust)
import qualified Data.SBV.Maybe as SBV

type SInput = (SInt16, SInt16)

data S = S
    { _currentRoom :: SInt16
    , _needLook :: SBool
    , _itemLocations :: SArray Int16 Int16
    , _dead :: SBool
    } deriving (Show, Generic, Mergeable)
makeLenses ''S

type Engine = ReaderT Game (WriterT [SString] (State S))

dirNames :: [String]
dirNames = ["North", "South", "East", "West", "Up", "Down"]

carried :: Int16
carried = 255

say :: SString -> Engine ()
say = tell . (:[])

say_ :: String -> Engine ()
say_ = say . literal

fillArray :: (Ix a, SymArray sarr, SymVal a, SymVal b) => Array a b -> sarr a b -> sarr a b
fillArray arr sarr = foldr write sarr (A.assocs arr)
  where
    write (i, x) sarr = writeArray sarr (literal i) (literal x)

initState :: Game -> SArray Int16 Int16 -> S
initState game itemsArr = S
    { _currentRoom = literal $ gameStartRoom game
    , _needLook = sTrue
    , _itemLocations = fillArray (fmap (\(Item _ _ _ loc) -> loc) $ gameItems game) itemsArr
    , _dead = sFalse
    }

runGame :: Game -> Engine a -> State S (a, [SString])
runGame game act = runWriterT $ runReaderT act game

stepWorld :: Engine (SMaybe Bool)
stepWorld = do
    perform (0, 0)
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
    let itemLocs' = [ (readArray itemLocs (literal item), desc) | (item, Item _ _ desc _) <- A.assocs items ]
        anyHere = sAny ((.== here) . fst) itemLocs'
    sWhen anyHere $ do
        say_ "I can also see:"
        forM_ itemLocs' $ \(loc, desc) -> sWhen (loc .== here) $ say $ literal $ " * " <> desc

score :: Engine SInt16
score = do
    treasury <- asks gameTreasury
    items <- asks gameItems
    itemLocs <- use itemLocations
    let treasureLocs = [ readArray itemLocs (literal item) | (item, Item True _ _ _) <- A.assocs items ]
    return $ count (.== literal treasury) treasureLocs

die :: Engine ()
die = dead .= sTrue

finished :: Engine (SMaybe Bool)
finished = do
    dead <- use dead

    maxScore <- asks gameMaxScore
    treasury <- asks gameTreasury
    items <- asks gameItems
    itemLocs <- use itemLocations
    let treasureLocs = [ readArray itemLocs (literal item) | (item, Item True _ _ _) <- A.assocs items ]
    let haveAllTreasure = map (.== literal treasury) treasureLocs `pbAtLeast` fromIntegral maxScore

    return $ ite dead (sJust sFalse) $
      ite haveAllTreasure (sJust sTrue) $
      sNothing

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

builtin :: SInput -> Engine SBool
builtin (verb, noun) = sCase verb
    [ (1, builtin_go)
    , (10, builtin_get)
    , (18, builtin_drop)
    ]
    (return sFalse)
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
        item <- parseItem
        ite (readArray locs item ./= here) (say_ "It's beyond my power to do that.") $ do
            move item (literal carried)
            say_ "OK."
        return sTrue

    builtin_drop = do
        locs <- use itemLocations
        here <- use currentRoom
        item <- parseItem
        ite (readArray locs item ./= literal carried) (say_ "It's beyond my power to do that.") $ do
            move item here
            say_ "OK."
        return sTrue

    parseItem = do
        items <- asks gameItems
        return $ SBV.fromMaybe (-1) $ sFindIndex (\(Item _ name _ _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items


perform :: SInput -> Engine ()
perform (verb, noun) = do
    handled <- performMatching (verb, noun)
    sUnless handled $ void $ builtin (verb, noun)
    return ()

type SCondition = (SInt16, SInt16)
type SInstr = SInt16

data SAction = SAction SInput [SCondition] [SInstr] deriving (Show, Generic, Mergeable)

sAction :: Action -> SAction
sAction (Action input conds instrs) = SAction (bimap literal literal input) (map (bimap literal literal) conds) (map literal instrs)

performMatching :: SInput -> Engine SBool
performMatching (verb, noun) = do
    actions <- asks $ fmap sAction . gameActions
    loop sFalse actions
  where
    loop handled [] = return handled
    loop handled (action@(SAction (verb', noun') conds instrs):actions) = do
        ite (sNot $ match (verb', noun')) (loop handled actions) $ do
            this <- do
                (bs, args) <- partitionEithers <$> mapM evalCond conds
                let b = sAnd bs
                ite b (exec args instrs) (return sFalse)
            ite this (ite auto (loop sTrue actions) (return sTrue)) (loop handled actions)

    match (verb', noun') = verb' .== verb .&& (auto .|| noun' .== 0 .|| noun' .== noun)
    auto = verb .== 0

evalCond :: (SInt16, SInt16) -> Engine (Either SBool SInt16)
evalCond (op, dat) = ite (op .== 0) (return $ Right dat) $ fmap Left $
    sCase op
      [ (1, isItemAt (literal carried))
      , (2, isItemAt =<< use currentRoom)
      , (4, uses currentRoom (.== dat))
      , (6, sNot <$> isItemAt (literal carried))
      , (12, sNot <$> itemAvailable)
      ]
      (return sTrue)
  where
    isItemAt room = do
        loc <- uses itemLocations (`readArray` dat)
        return $ loc .== room

    itemAvailable = do
        loc <- uses itemLocations (`readArray` dat)
        here <- use currentRoom
        return $ loc .== literal carried .|| loc .== here

type Exec = StateT [SInt16] Engine

nextArg :: Exec SInt16
nextArg = do
    xs <- get
    case xs of
        (x:xs) -> do
            put xs
            return x
        [] -> error "Out of args"

exec :: [SInt16] -> [SInstr] -> Engine SBool
exec args instrs = flip evalStateT args $ do
    mapM_ execInstr instrs
    return sTrue

execInstr :: SInstr -> Exec SBool
execInstr instr =
    ite (1 .<= instr .&& instr .<= 51) (msg instr) $
    ite (102 .<= instr) (msg $ instr - 50) $
    sCase instr
        [ (0, return sTrue)
        , (54, teleport)
        , (63, gameOver)
        , (64, lookAround)
        , (65, showScore)
        , (66, showInventory)
        , (72, swapItems)
        ]
        (return sTrue)
  where
    handler act = act *> return sTrue

    msg i = handler $ lift $ do
        s <- asks $ (.! i) . fmap literal . gameMessages
        say s

    teleport = handler $ do
        room <- nextArg
        lift $ currentRoom .= room

    gameOver = handler $ lift die

    showScore = handler $ lift $ say_ "SCORE"
    showInventory = handler $ lift $ say_ "INVENTORY"
    lookAround = handler $ lift look

    swapItems = handler $ do
        item1 <- nextArg
        item2 <- nextArg
        lift $ do
            loc1 <- uses itemLocations (`readArray` item1)
            loc2 <- uses itemLocations (`readArray` item2)
            move item1 loc2
            move item2 loc1

move :: SInt16 -> SInt16 -> Engine ()
move item loc = itemLocations %= \arr -> writeArray arr item loc
