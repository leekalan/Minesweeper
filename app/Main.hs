{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad
import Text.Read (readMaybe)

import Print
import Game
import Board
import Utils.State
import Utils.Transformer
import Utils.Environment
import Data.Foldable

data Action = Reveal (Int, Int) | Flag (Int, Int)

emptyCell :: Cell
emptyCell = Cell (Safe 0) False False

revealAll :: State Board ()
revealAll = do
  board <- get
  put $ map (map (\c -> c { isRevealed = True })) board

updateAt :: Int -> (a -> a) -> [a] -> [a]
updateAt i f xs =
  [ if idx == i then f x else x | (x, idx) <- zip xs [0..] ]

revealPos :: (Int, Int) -> State Board ()
revealPos (r,c) = do
  board <- get
  put $ updateAt r (updateAt c revealCell) board
  where
    revealCell :: Cell -> Cell
    revealCell cell = cell { isRevealed = True }

autoRevealRecurse :: State Board Bool
autoRevealRecurse = do
  b <- get
  foldrM f False $ n b
  where
    complete b = concatMap (\(rowIdx, row) ->
      [ (rowIdx, colIdx)
      | (colIdx, x) <- zip [0..] row, isComplete x (rowIdx, colIdx) b ]
      ) $ zip [0..] b

    isComplete :: Cell -> (Int, Int) -> Board -> Bool
    isComplete (Cell (Safe adj) True _) pos b = (adj - countAdjFlags b pos) <= 0
    isComplete _ _ _ = False

    n board = concatMap (neighbours $ boardSize board) $ complete board

    f :: (Int, Int) -> Bool -> State Board Bool
    f pos changed = do
      board <- get
      let cell = board !! fst pos !! snd pos
      if not $ isFlagged cell || isRevealed cell then do
        revealPos pos
        return True
      else return changed

autoReveal :: State Board ()
autoReveal = do
  changed <- autoRevealRecurse
  when changed autoReveal

flagPos :: (Int, Int) -> State Board ()
flagPos (r,c) = do
  b <- get
  put $ updateAt r (updateAt c flagCell) b
  where
    flagCell :: Cell -> Cell
    flagCell cell = cell { isFlagged = not $ isFlagged cell }

getValidInput :: IO (Maybe (Int, Int, Int))
getValidInput = do
  putStrLn "Enter rows, cols and mines (q to quit): "
  ws <- fmap words getLine

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
  ws <- fmap words getLine

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
            ws' <- fmap words getLine
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

  state <- get
  let board = getBoard state

  result <- lift $ getAction (boardSize board)

  nest <- forM result $ \case
    Reveal pos -> if isValidAction board pos then do
      elev $ mapEnvMut getBoard setBoard $ revealPos pos
      postMutation
    else do
      lift $ putStrLn "The cell is already revealed."
      gameLoop
    Flag pos -> if isValidAction board pos then do
      elev $ mapEnvMut getBoard setBoard $ flagPos pos
      postMutation
    else do
      lift $ putStrLn "The cell cannot be flagged."
      gameLoop

  return $ join nest

  where
    isValidAction :: Board -> (Int, Int) -> Bool
    isValidAction board pos = not (isRevealed cell)
      where cell = board !! fst pos !! snd pos

    isWin :: GameStateM Bool
    isWin = fmap (all (all safeRevealed) . getBoard) get
      where
        safeRevealed :: Cell -> Bool
        safeRevealed (Cell (Safe _) False _) = False
        safeRevealed _ = True

    isLose :: GameStateM Bool
    isLose = fmap (any (any revealedMine) . getBoard) get
      where
        revealedMine :: Cell -> Bool
        revealedMine (Cell Mine True _) = True
        revealedMine _ = False

    postMutation :: GameStateMT IO (Maybe Bool)
    postMutation = do
      elev $ mapEnvMut getBoard setBoard autoReveal
      win <- elev isWin
      lose <- elev isLose
      if win then do
        return $ Just True
      else if lose then do
        return $ Just False
      else do
        gameLoop

main :: IO ()
main = do
  result <- getValidInput

  case result of
    Nothing -> putStrLn "Quitting.."
    Just (rows, cols, mines) -> do
      game <- newGame rows cols mines
      (result', state) <- runStateT gameLoop game
      case result' of
        Nothing -> putStrLn "Quitting.."
        Just end -> do
          printBoard (evalState revealAll $ getBoard state)
          if end then do
            putStrLn $ "You won! (" ++ show mines ++ " mines cleared)"
          else do
            putStrLn $ "You lost! (" ++ show (getMinesLeft state - countFlags (getBoard state)) ++ " mines left)"
