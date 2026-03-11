module UI.Render
  ( render
  , renderCell
  ) where

import Game.Board (Board, boardSize, getCell)
import Game.State (GameState(..))
import Game.Types (Cell(..), Player(..))

ansiReset, ansiRed, ansiBlue, ansiDim :: String
ansiReset = "\ESC[0m"
ansiRed   = "\ESC[31m"
ansiBlue  = "\ESC[34m"
ansiDim   = "\ESC[2m"

render :: GameState -> String
render st =
  let b = gsBoard st
      (rows, cols) = boardSize b

      turnLine =
        "Turn: " ++ show (gsTurn st) ++ "    k=" ++ show (gsK st) ++ "\n"

      header =
        "    " ++ concatMap (\c -> padIndex c ++ "  ") [0 .. cols - 1] ++ "\n"

      sepLine =
        "   +" ++ concat (replicate cols "---+") ++ "\n"

      rowLine r =
        " " ++ padIndex r ++ " |"
        ++ concatMap (\c -> " " ++ renderCell (getCell b (r, c)) ++ " |") [0 .. cols - 1]
        ++ "\n"

      boardLines =
        sepLine ++ concatMap (\r -> rowLine r ++ sepLine) [0 .. rows - 1]

  in turnLine ++ "\n" ++ header ++ boardLines

padIndex :: Int -> String
padIndex n
  | n < 10    = show n
  | otherwise = show n

renderCell :: Maybe Cell -> String
renderCell Nothing           = " "
renderCell (Just Empty)      = ansiDim  ++ "·" ++ ansiReset
renderCell (Just (Taken P1)) = ansiRed  ++ "X" ++ ansiReset
renderCell (Just (Taken P2)) = ansiBlue ++ "O" ++ ansiReset