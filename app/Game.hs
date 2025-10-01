module Game where

import Board
import State

data GameState = GameState {
    getMinesLeft :: Int,
    getBoard :: Board
}

type GameStateM a = State GameState a

type GameStateMT m a = StateT GameState m a

newGame :: Int -> Int -> Int -> IO GameState
newGame rows cols mines = do
    board <- placeMines rows cols mines
    return $ GameState mines board

updateMinesLeft :: GameStateM ()
updateMinesLeft = do
    state <- get
    let b = getBoard state
    let minesLeft = countMines b - countFlags b
    put $ state { getMinesLeft =  minesLeft}
