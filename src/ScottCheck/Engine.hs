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
    , _needLook :: SBool
    , _itemLocations :: SArray Int16 Int16
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

initState :: Game -> SArray Int16 Int16 -> S
initState game arr = S
    { _currentRoom = literal $ gameStartRoom game
    , _needLook = sTrue
    , _itemLocations = fillArray (fmap (\(Item _ _ _ loc) -> loc) $ gameItems game) arr
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
    let itemLocs' = [ (readArray itemLocs (literal item), desc) | (item, Item _ _ desc _) <- A.assocs items ]
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
        ite (readArray locs item ./= here) (say_ "It's beyond my power to do that.") $ do
            move item (literal carried)
            say_ "OK."

    builtin_drop = do
        locs <- use itemLocations
        here <- use currentRoom
        item <- parseItem
        ite (readArray locs item ./= literal carried) (say_ "It's beyond my power to do that.") $ do
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

exec :: [SInt16] -> [SInstr] -> Engine ()
exec args instrs = flip evalStateT args $ mapM_ execInstr instrs

execInstr :: SInstr -> Exec ()
execInstr instr =
    ite (1 .<= instr .&& instr .<= 51) (msg instr) $
    ite (102 .<= instr) (msg $ instr - 50) $
    sCase instr
        [ (0, return ())
        , (54, teleport)
        , (63, gameOver)
        , (64, lookAround)
        , (65, showScore)
        , (66, showInventory)
        , (72, swapItems)
        ]
        (return ())
  where
    msg i = lift $ do
        s <- asks $ (.! i) . fmap literal . gameMessages
        say s

    teleport = do
        room <- nextArg
        lift $ currentRoom .= room

    gameOver = lift die

    showScore = lift $ do
        treasury <- asks gameTreasury
        items <- asks gameItems
        itemLocations <- use itemLocations
        let treasureLocs = [ readArray itemLocations (literal item) | (item, Item True _ _ _) <- A.assocs items ]
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
            loc1 <- uses itemLocations (`readArray` item1)
            loc2 <- uses itemLocations (`readArray` item2)
            move item1 loc2
            move item2 loc1

move :: SInt16 -> SInt16 -> Engine ()
move item loc = itemLocations %= \arr -> writeArray arr item loc
