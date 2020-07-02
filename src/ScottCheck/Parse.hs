{-# LANGUAGE RecordWildCards, ViewPatterns #-}
module ScottCheck.Parse where

import ScottCheck.GameData

import Text.Parser.Combinators
import Text.Parser.Char
import Data.Attoparsec.Text (Parser, signed, decimal)
import Control.Monad (replicateM, replicateM_)
import qualified Data.Map as M
import Data.Array as A
import Data.Int (Int16)

type P = Parser

lexeme :: P a -> P a
lexeme parser = parser <* spaces

num :: P Int16
num = lexeme $ signed decimal <?> "Number"

str :: P String
str = lexeme $ many (notChar '"') `surroundedBy` char '"' <?> "String"

cardinality :: P Int16
cardinality = (1 + ) <$> num

action :: P Action
action = Action <$> input <*> replicateM 5 cond <*> (mconcat <$> replicateM 2 instr)
  where
    input = (`divMod` 150) <$> num
    cond = (`modDiv` 20) <$> num
    instr = flatten <$> num
      where
        flatten 0 = []
        flatten x = [x1, x2] where (x1, x2) = x `divMod` 150

x `modDiv` n = let (q, r) = x `divMod` n in (r, q)

room :: P Room
room = Room <$> replicateM 6 num <*> str

item :: M.Map String Int16 -> P Item
item nouns = do
    s <- str
    startLoc <- num
    let (desc, (drop 1 -> rest)) = break (== '/') s
        nameStr = if null rest then Nothing else Just $ takeWhile (/= '/') rest
    name <- traverse lookupNoun nameStr
    let isTreasure = case desc of '*':_ -> True; _ -> False
    return $ Item isTreasure name desc startLoc
  where
    lookupNoun :: String -> P Int16
    lookupNoun s = maybe (fail $ "Item name not a noun: " <> s) return $ M.lookup s nouns

parseArray :: Int16 -> P a -> P (Array Int16 a)
parseArray n parse = listArray (0, n - 1) <$> replicateM (fromIntegral n) parse

game :: P Game
game = do
    _ <- num
    numItems <- cardinality
    numActions <- cardinality
    gameDictSize <- cardinality
    numRooms <- cardinality
    gameMaxLoad <- num
    gameStartRoom <- num
    gameMaxScore <- num
    gameWordLength <- num
    gameLampTime <- num
    numMessages <- cardinality
    gameTreasury <- num

    gameActions <- replicateM (fromIntegral numActions) action
    (verbs, nouns) <- unzip <$> replicateM(fromIntegral gameDictSize) ((,) <$> str <*> str)
    let gameVerbsRaw = listArray (0, fromIntegral gameDictSize - 1) verbs
        gameVerbs = M.fromList $ zip verbs [0..]
        gameNounsRaw = listArray (0, fromIntegral gameDictSize - 1) nouns
        gameNouns = M.fromList $ zip nouns [0..]

    gameRooms <- parseArray numRooms room
    gameMessages <- parseArray numMessages str
    gameItems <- parseArray numItems (item gameNouns)

    replicateM_ (fromIntegral numActions) str
    replicateM_ 3 num -- TODO

    eof

    return Game{..}
