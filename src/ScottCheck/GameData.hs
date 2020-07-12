module ScottCheck.GameData where

import Data.Int
import Data.Array (Array)
import Data.Map (Map)
import Data.SBV
import GHC.Generics

data Game = Game
    { gameStartRoom :: Int16
    , gameTreasury :: Int16
    , gameMaxScore :: Int16
    , gameDictSize :: Int16
    , gameItems :: Array Int16 Item
    , gameActions :: [Action]
    , gameRooms :: Array Int16 Room
    , gameMessages :: Array Int16 String
    }
    deriving (Show)

type Input = (Int16, Int16)
type Condition = (Int16, Int16)
type Instr = Int16

data Action = Action Input [Condition] [Instr] deriving (Show)

data Room = Room [Int16] String deriving (Show)

data Item = Item Bool (Maybe Int16) String Int16 deriving (Show)
