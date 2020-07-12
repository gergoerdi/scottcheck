module Main where

import ScottCheck.Engine

import Data.SBV hiding (options, solve)
import Data.SBV.Control
import qualified Data.SBV.Tuple as SBV
import qualified Data.Array as A


loopState :: (SymVal i) => (Int -> Query (SBV i)) -> s -> (SBV i -> s -> (SBool, s)) -> Query [i]
loopState genCmd s0 step = go 1 s0 []
  where
    go i s cmds = do
        io $ putStrLn $ "Searching at depth: " ++ show i

        cmd <- genCmd i
        let cmds' = cmds ++ [cmd]

        push 1
        let (finished, s') = step cmd s
        constrain finished
        cs <- checkSat

        case cs of
            Unk -> error $ "Solver said Unknown, depth: " ++ show i
            Unsat -> do
                pop 1
                go (i+1) s' cmds'
            Sat -> mapM getValue cmds'

solve :: Game -> IO ()
solve theGame = do
    cmds <- runSMT $ do
        arr <- newArray "items" Nothing
        query $ loopState genInput (initState theGame arr) $ \cmd -> do
            let (verb, noun) = SBV.untuple cmd
            stepPlayer theGame (verb, noun)

    mapM_ print cmds
  where
    genWord = do
        word <- freshVar_
        constrain $ 0 .<= word .&& word .< literal (gameDictSize theGame)
        return word
    genInput i = do
        verb <- genWord
        noun <- genWord
        return $ SBV.tuple (verb, noun)

main :: IO ()
main = do
    let theGame = Game
            { gameStartRoom = 1
            , gameTreasury = 1
            , gameDictSize = 19
            , gameItems = A.listArray (0,0)
                  [ Item (Just 7) 255
                  ]
            }

    solve theGame
