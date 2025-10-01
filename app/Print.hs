module Print where

import Operators

import Game
import Board
import State

printGame :: GameStateMT IO ()
printGame = do
    state <- getT
    let board = getBoard state
    let minesLeft = getMinesLeft state
    liftState $ printBoard board
    liftState $ putStrLn $ "Mines left: " ++ show minesLeft ++ "\n"

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
