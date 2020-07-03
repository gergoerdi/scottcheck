{-# LANGUAGE RecordWildCards, TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
module ScottCheck.Engine where

import ScottCheck.GameData
import ScottCheck.Utils

import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.Writer
import Data.SBV.MTL ()

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

data S = S
    { _currentRoom :: SInt16
    , _needLook :: SBool
    , _itemLocations :: Array Int16 SInt16
    , _dead :: SBool
    } deriving (Show, Generic, Mergeable)
makeLenses ''S

type Engine = ReaderT Game (StateT S (Writer [SString]))

dirNames :: [String]
dirNames = ["North", "South", "East", "West", "Up", "Down"]

carried :: Int16
carried = 255

say :: SString -> Engine ()
say = lift . lift . tell . (:[])

say_ :: String -> Engine ()
say_ = say . literal

runGame :: Game -> Engine a -> Writer [SString] a
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
        let item = SBV.fromMaybe (-1) $ sFindIndex (\(Item _ name _ _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items
        ite (select (A.elems locs) (-1) item ./= here) (say_ "It's beyond my power to do that.") $ do
            move item (literal carried)
            say_ "OK."
        return sTrue

    builtin_drop = do
        locs <- use itemLocations
        here <- use currentRoom
        items <- asks gameItems
        let item = SBV.fromMaybe (-1) $ sFindIndex (\(Item _ name _ _) -> maybe sFalse ((noun .==) . literal) name) $ A.elems items
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


move :: SInt16 -> SInt16 -> Engine ()
move item loc = itemLocations %= replaceAt item loc
