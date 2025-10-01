{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Text.Read (readMaybe)

import Operators

import Print
import Game
import Board
import State

data Action = Reveal (Int, Int) | Flag (Int, Int)

mutateBoard :: (Board -> Board) -> GameStateM ()
mutateBoard f = do
    state <- get
    let b = f $ getBoard state
    put $ state { getBoard = b }

emptyCell :: Cell
emptyCell = Cell (Safe 0) False False

revealAll :: Board -> Board
revealAll = map (map (\c -> c { isRevealed = True }))

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs =
    [ if idx == i then f x else x | (x, idx) <- zip xs [0..] ]

revealPos :: (Int, Int) -> Board -> Board
revealPos (r,c) = updateAt r (updateAt c revealCell)
    where
        revealCell :: Cell -> Cell
        revealCell cell = cell { isRevealed = True }

autoRevealRecurse :: Board -> (Board, Bool)
autoRevealRecurse b = foldr f (b, False) n
    where
        complete = concatMap (\(rowIdx, row) ->
            [ (rowIdx, colIdx)
            | (colIdx, x) <- zip [0..] row, isComplete x (rowIdx, colIdx) ]
            ) $ zip [0..] b

        isComplete :: Cell -> (Int, Int) -> Bool
        isComplete (Cell (Safe adj) True _) pos = (adj - countAdjFlags b pos) <= 0
        isComplete _ _ = False

        n = concat $ neighbours (boardSize b) <<| complete

        f :: (Int, Int) -> (Board, Bool) -> (Board, Bool)
        f pos (board, changed) = if not (isFlagged cell || isRevealed cell) then
            (revealPos pos board, True) else (board, changed)
            where cell = board !! fst pos !! snd pos;

autoReveal :: Board -> Board
autoReveal b = if changed then autoReveal board else board
    where (board, changed) = autoRevealRecurse b

flagPos :: (Int, Int) -> Board -> Board
flagPos (r,c) = updateAt r (updateAt c flagCell)
    where
        flagCell :: Cell -> Cell
        flagCell cell = cell { isFlagged = not $ isFlagged cell }

getValidInput :: IO (Maybe (Int, Int, Int))
getValidInput = do
    putStrLn "Enter rows, cols and mines (q to quit): "
    ws <- words <<| getLine

    case ws of
        ["q"] -> return Nothing
        [rStr, cStr, mStr] -> do
            case (readMaybe rStr, readMaybe cStr, readMaybe mStr) of
                (Just rows, Just cols, Just mines) ->
                    if rows <= 0 || cols <= 0 || mines < 0 || mines > rows * cols
                        then do
                            putStrLn "Rows and columns must be positive and 0 <= mines <= rows*cols."
                            getValidInput
                        else return $ Just (rows, cols, mines)
                _ -> do
                    putStrLn "All inputs must be numbers."
                    getValidInput
        _ -> do
            putStrLn "Invalid input. You must enter exactly 3 numbers."
            getValidInput

getAction :: (Int, Int) -> IO (Maybe Action)
getAction size = do
    putStrLn "Enter row and column (q to quit): "
    ws <- words <<| getLine

    case ws of
        ["q"] -> return Nothing
        [rStr, cStr] -> do
            case (readMaybe rStr, readMaybe cStr) of
                (Just r', Just c') ->
                    let (r, c) = (r'-1,c'-1)
                    in if not $ inBounds size (r, c) then do
                        putStrLn "Row and column must be in bounds."
                        getAction size
                    else do
                        putStrLn "Reveal, flag or cancel? (r/f/c): "
                        ws' <- words <<| getLine
                        case ws' of
                            ["r"] -> return $ Just $ Reveal (r, c)
                            ["f"] -> return $ Just $ Flag (r, c)
                            ["c"] -> do
                                putStrLn "Canceled."
                                getAction size
                            _ -> do
                                putStrLn "Invalid input. You must enter 'r' or 'f'."
                                getAction size
                _ -> do
                    putStrLn "Row and column must be numbers."
                    getAction size
        _ -> do
            putStrLn "Invalid input. You must enter exactly 2 numbers."
            getAction size

gameLoop :: GameStateMT IO (Maybe Bool)
gameLoop = do
    printGame

    state <- getT
    let board = getBoard state

    result <- liftState $ getAction (boardSize board)

    nest <- forM result $ \case
        Reveal pos -> if isValidAction board pos then do
            lift $ mutateBoard (revealPos pos)
            postMutation
        else do
            liftState $ putStrLn "The cell is already revealed."
            gameLoop
        Flag pos -> if isValidAction board pos then do
            lift $ mutateBoard (flagPos pos)
            postMutation
        else do
            liftState $ putStrLn "The cell cannot be flagged."
            gameLoop

    return $ join nest

    where
        isValidAction :: Board -> (Int, Int) -> Bool
        isValidAction board pos = not (isRevealed cell)
            where cell = board !! fst pos !! snd pos

        isWin :: GameStateM Bool
        isWin = fmap (and <~ map (and <~ map safeRevealed) <~ getBoard) get
            where
                safeRevealed :: Cell -> Bool
                safeRevealed (Cell (Safe _) False _) = False
                safeRevealed _ = True

        isLose :: GameStateM Bool
        isLose = fmap (or <~ map (or <~ map revealedMine) <~ getBoard) get
            where
                revealedMine :: Cell -> Bool
                revealedMine (Cell Mine True _) = True
                revealedMine _ = False

        postMutation :: GameStateMT IO (Maybe Bool)
        postMutation = do
            lift $ mutateBoard autoReveal
            win <- lift isWin
            lose <- lift isLose
            if win then do
                return $ Just True
            else if lose then do
                return $ Just False
            else do
                gameLoop

main :: IO ()
main = do
    result <- getValidInput

    forElseOr result (putStrLn "Quitting..") $ \(rows, cols, mines) -> do
        game <- newGame rows cols mines
        (result', state) <- runStateT gameLoop game
        forElseOr result' (putStrLn "Quitting..") $ \end -> do
            printBoard (revealAll $ getBoard state)
            if end then do
                putStrLn $ "You won! (" ++ show mines ++ " mines cleared)"
            else do
                putStrLn $ "You lost! (" ++ show (getMinesLeft state - countFlags (getBoard state)) ++ " mines left)"
