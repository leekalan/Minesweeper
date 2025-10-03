module Board where

import System.Random
import Utils.State

type Board = [[Cell]]

data Cell = Cell {
  getCell :: CellType,
  isRevealed :: Bool,
  isFlagged :: Bool
}

data CellType = Safe { adjMines :: Int } | Mine

boardHeight :: Board -> Int
boardHeight = length

boardWidth :: Board -> Int
boardWidth = length . head

boardSize :: Board -> (Int, Int)
boardSize b = (boardHeight b, boardWidth b)

inBounds :: (Int, Int) -> (Int, Int) -> Bool
inBounds (h,w) (r0,c0) =
  r0 >= 0 && r0 < h &&
  c0 >= 0 && c0 < w

toCoords :: Int -> Int -> (Int, Int)
toCoords cols idx = (idx `div` cols, idx `mod` cols)

placeMines :: Int -> Int -> Int -> IO Board
placeMines rows cols mines = do
  let total = rows * cols
  positions' <- randomPositions total mines
  let positions = map (toCoords cols) positions'
  let isMine idx = idx `elem` positions

  let board = [[ Cell (if isMine (r, c) then Mine else Safe 0) False False
        | c <- [0..cols-1] ] | r <- [0..rows-1] ]

  return $ evalState calcAdj board

calcAdj :: State Board ()
calcAdj = do
  b <- get
  put $ [ [ updateCell b (r, c) cell
    | (c, cell) <- zip [0..] row ]
    | (r, row) <- zip [0..] b ]
  where
    updateCell :: Board -> (Int, Int) -> Cell -> Cell
    updateCell b pos (Cell (Safe _) _ _) =
      Cell (Safe $ countAdjMines b pos) False False
    updateCell _ _ cell = cell

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
countMines = sum . map (length . filter isMine)
  where
    isMine (Cell Mine _ _) = True
    isMine _               = False

countFlags :: Board -> Int
countFlags = sum . map (length . filter flagged)
  where
    flagged (Cell _ _ True) = True
    flagged _               = False
