-- src/UI/Render.hs
module UI.Render
  ( render
  , renderCell
  ) where

import Data.List (intercalate)

import Game.Board (Board, boardSize, getCell)
import Game.State (GameState(..))
import Game.Types (Cell(..), Player(..))

ansiReset, ansiRed, ansiBlue, ansiDim :: String
ansiReset = "\ESC[0m"
ansiRed   = "\ESC[31m"
ansiBlue  = "\ESC[34m"
ansiDim   = "\ESC[2m"


-- Builds the full text display of the current game, including the title, column labels, and the formatted board grid.
render :: GameState -> String
render st =
  let b = gsBoard st
      (rows, cols) = boardSize b

      rowLabelW = length (show (rows - 1))
      colLabelW = length (show (cols - 1))

      innerW = max 1 colLabelW
      cellW  = innerW + 2

      title =
        "Turn: " ++ show (gsTurn st)
        ++ "    k=" ++ show (gsK st)
        ++ "    Board: " ++ show rows ++ "x" ++ show cols

      header =
        replicate (rowLabelW + 2) ' '
        ++ intercalate " " (map (center cellW . show) [0 .. cols - 1])

      sep =
        replicate rowLabelW ' '
        ++ " +"
        ++ concatMap (\_ -> replicate cellW '-' ++ "+") [1 .. cols]

      rowLine r =
        padLeft rowLabelW (show r)
        ++ " |"
        ++ intercalate "|" [cellBlock innerW (cellAt r c) | c <- [0 .. cols - 1]]
        ++ "|"

      cellAt r c =
        case getCell b (r, c) of
          Just cell -> cell
          Nothing   -> Empty

      boardLines =
        concatMap (\r -> [sep, rowLine r]) [0 .. rows - 1] ++ [sep]

  in unlines (title : "" : header : boardLines)


-- Formats one board cell so its symbol is centered inside a fixed-width box.
cellBlock :: Int -> Cell -> String
cellBlock innerW cell =
  let sym = renderCell cell
      visibleSymW = 1
      leftPad  = (innerW - visibleSymW) `div` 2
      rightPad = innerW - visibleSymW - leftPad
  in " " ++ replicate leftPad ' ' ++ sym ++ replicate rightPad ' ' ++ " "


-- Converts a cell’s contents into the colored symbol shown on the screen.
renderCell :: Cell -> String
renderCell Empty       = ansiDim  ++ "·" ++ ansiReset
renderCell (Taken P1)  = ansiRed  ++ "X" ++ ansiReset
renderCell (Taken P2)  = ansiBlue ++ "O" ++ ansiReset

padLeft :: Int -> String -> String
padLeft w s = replicate (w - length s) ' ' ++ s

center :: Int -> String -> String
center w s =
  let total = w - length s
      l = total `div` 2
      r = total - l
  in replicate l ' ' ++ s ++ replicate r ' '