{-# LANGUAGE LambdaCase #-}
module Main where

import System.Random

import Control.Monad
import Text.Read
import Operators

data GameState = GameState {
    getMinesLeft :: Int,
    getBoard :: Board
}

type Board = [[Cell]]

data Cell = Cell {
    getCell :: CellType,
    isRevealed :: Bool,
    isFlagged :: Bool
}

data CellType = Safe { adjMines :: Int } | Mine

data Action = Reveal (Int, Int) | Flag (Int, Int)

newGame :: Int -> Int -> Int -> IO GameState
newGame rows cols mines = do
    board <- placeMines rows cols mines
    return $ GameState mines board

printGame :: GameState -> IO ()
printGame (GameState minesLeft board) = do
    printBoard board
    putStrLn $ "Mines left: " ++ show minesLeft

mutateBoard :: GameState -> (Board -> Board) -> GameState
mutateBoard (GameState _ board) f =
    let b = f board
    in GameState (countMines b - countFlags b) b

emptyCell :: Cell
emptyCell = Cell (Safe 0) False False

boardHeight :: Board -> Int
boardHeight = length

boardWidth :: Board -> Int
boardWidth = length <~ head

boardSize :: Board -> (Int, Int)
boardSize b = (boardHeight b, boardWidth b)

inBounds :: (Int, Int) -> (Int, Int) -> Bool
inBounds (h,w) (r0,c0) =
    r0 >= 0 && r0 < h &&
    c0 >= 0 && c0 < w

placeMines :: Int -> Int -> Int -> IO Board
placeMines rows cols mines = do
    let total = rows * cols
    positions <- (toCoords cols <<|) <<| randomPositions total mines
    let isMine idx = idx `elem` positions

    let board = [[ Cell (if isMine (r, c) then Mine else Safe 0) False False
            | c <- [0..cols-1] ] | r <- [0..rows-1] ]

    return $ fillAdj board

toCoords :: Int -> Int -> (Int, Int)
toCoords cols idx = (idx `div` cols, idx `mod` cols)

fillAdj :: Board -> Board
fillAdj b =
        [ [ updateCell (r, c) cell
        | (c, cell) <- zip [0..] row ]
        | (r, row) <- zip [0..] b ]
    where
        updateCell :: (Int, Int) -> Cell -> Cell
        updateCell pos (Cell (Safe _) _ _) =
            Cell (Safe $ countAdjMines b pos) False False
        updateCell _ cell = cell

neighbours :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
neighbours (rows, cols) (r,c) =
    [ (r', c')
    | dr <- [-1..1]
    , dc <- [-1..1]
    , (dr,dc) /= (0,0)
    , let r' = r + dr
    , let c' = c + dc
    , inBounds (rows, cols) (r', c')
    ]

countAdjMines :: Board -> (Int, Int) -> Int
countAdjMines board pos = length
    [ ()
    | rc <- neighbours (boardSize board) pos
    , isMine (board !! fst rc !! snd rc)
    ]
    where
        isMine :: Cell -> Bool
        isMine (Cell Mine _ _) = True
        isMine _               = False

countAdjFlags :: Board -> (Int, Int) -> Int
countAdjFlags board pos = length
    [ ()
    | rc <- neighbours (boardSize board) pos
    , flagged (board !! fst rc !! snd rc)
    ]
    where
        flagged :: Cell -> Bool
        flagged (Cell _ _ True) = True
        flagged _               = False

randomPositions :: Int -> Int -> IO [Int]
randomPositions total k = do
    shuffled <- shuffle [0..total-1]
    return (take k shuffled)

shuffle :: [a] -> IO [a]
shuffle [] = return []
shuffle xs = do
    i <- randomRIO (0, length xs - 1)
    let (left, right0) = splitAt i xs
        (a, right) = case right0 of
            (y:ys) -> (y, ys)
            [] -> error "unreachable: index out of bounds"
    rest <- shuffle (left ++ right)
    return (a : rest)

countMines :: Board -> Int
countMines = sum <~ map (length <~ filter isMine)
    where
        isMine (Cell Mine _ _) = True
        isMine _               = False

countFlags :: Board -> Int
countFlags = sum <~ map (length <~ filter flagged)
    where
        flagged (Cell _ _ True) = True
        flagged _               = False

printBoard :: Board -> IO ()
printBoard b = do
    let n = length $ head b
    putChar '\n'
    printColNumbers 1 n
    putStr "   ┌─"
    printHorizontalBar '┴' 1 n
    putStrLn "┐"
    mapM_ (uncurry printRow) $ zip b [1..]
    putStr "   └─"
    printHorizontalBar '┬' 1 n
    putStrLn "┘\n"
    where
        padR :: Int -> String -> String
        padR n str =
            let padding = n - length str
            in if padding < 0 then drop (-padding) str
            else str ++ replicate padding ' '

        padL :: Int -> String -> String
        padL n str =
            let padding = n - length str
            in if padding < 0 then drop (-padding) str
            else replicate padding ' ' ++ str

        printHorizontalBar :: Char -> Int -> Int -> IO ()
        printHorizontalBar c i n = case i of
            _ | i > n -> return () | otherwise -> do
                if i == 1 || i `mod` 5 == 0 then putChar c >> putChar '─'
                else putStr "──"
                printHorizontalBar c (i+1) n

        printColNumbers :: Int -> Int -> IO ()
        printColNumbers i n = do
            case i of
                _ | i == 1 -> putStr ("     " ++ padR 7 "1") >> printColNumbers (i+4) n
                  | i > n -> putChar '\n'
                  | otherwise -> putStr (" " ++ padR 9 (show i)) >> printColNumbers (i+5) n

        printRow :: [Cell] -> Int -> IO ()
        printRow r i = do
            if i == 1 || i `mod` 5 == 0 then putStr $ padL 2 (show i) ++ " ┼ "
            else putStr "   │ "
            mapM_ (showCell ~> \c -> putChar c >> putChar ' ') r
            if i == 1 || i `mod` 5 == 0 then putStrLn "┼" else putStrLn "│"

showCell :: Cell -> Char
showCell (Cell Mine True _) = '*'                   -- revealed mine
showCell (Cell (Safe 0) True _) = ' '               -- revealed safe with 0 adjacent mines
showCell (Cell (Safe n) True _) = head $ show n     -- revealed safe with n adjacent mines
showCell (Cell _ _ True) = 'F'                      -- flagged
showCell _ = '#'                                    -- unrevealed

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

gameLoop :: GameState -> IO (Maybe (GameState, Bool))
gameLoop game = do
    printBoard (getBoard game)

    result <- getAction (boardSize $ getBoard game)
    nest <- forM result $ \case
        Reveal pos -> if isValidAction pos then do
            let game' = mutateBoard game (revealPos pos)
            postMutation game'
        else do
            putStrLn "The cell is already revealed."
            gameLoop game
        Flag pos -> if isValidAction pos then do
            let game' = mutateBoard game (flagPos pos)
            postMutation game'
        else do
            putStrLn "The cell cannot be flagged."
            gameLoop game

    return $ join nest

    where
        isValidAction :: (Int, Int) -> Bool
        isValidAction pos = not (isRevealed cell)
            where cell = getBoard game !! fst pos !! snd pos

        isWin :: GameState -> Bool
        isWin = and <~ map (and <~ map safeRevealed) <~ getBoard
            where
                safeRevealed :: Cell -> Bool
                safeRevealed (Cell (Safe _) False _) = False
                safeRevealed _ = True

        isLose :: GameState -> Bool
        isLose = or <~ map (or <~ map revealedMine) <~ getBoard
            where
                revealedMine :: Cell -> Bool
                revealedMine (Cell Mine True _) = True
                revealedMine _ = False
        postMutation game' = do
            let game'' = mutateBoard game' autoReveal
            if isWin game'' then do
                return $ Just (game'', True)
            else if isLose game'' then do
                return $ Just (game'', False)
            else do
                gameLoop game''

main :: IO ()
main = do
    result <- getValidInput
    forElseOr result (putStrLn "Quitting..") $ \(rows, cols, mines) -> do
        game <- newGame rows cols mines
        result' <- gameLoop game
        forElseOr result' (putStrLn "Quitting..") $ \(state, end) -> do
            printBoard (revealAll $ getBoard state)
            if end then do
                putStrLn $ "You won! (" ++ show mines ++ " mines cleared)"
            else do
                putStrLn $ "You lost! (" ++ show (getMinesLeft state - countFlags (getBoard state)) ++ " mines left)"
