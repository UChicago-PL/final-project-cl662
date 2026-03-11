-- src/UI/Loop.hs
module UI.Loop
  ( run
  ) where

import Game.Board (getCell, setCell)
import Game.Rules (Outcome(..), checkOutcome)
import Game.State (GameState(..), initState)
import Game.Types (Cell(..), opponent)

import UI.Ansi (clearScreenSafe)
import UI.Input (promptPos)
import UI.Render (render)

run :: IO ()
run = roundLoop 1

roundLoop :: Int -> IO ()
roundLoop roundNo = do
  let st0 = initState 3 3 3
  gameLoop roundNo st0

gameLoop :: Int -> GameState -> IO ()
gameLoop roundNo st = do
  clearScreenSafe
  putStrLn ("mnk-game  |  Round " ++ show roundNo)
  putStrLn ""
  putStrLn (render st)

  case checkOutcome (gsK st) (gsBoard st) of
    Winner p -> do
      putStrLn ("\nResult: " ++ show p ++ " wins.")
      playAgain roundNo
    Draw -> do
      putStrLn "\nResult: Draw."
      playAgain roundNo
    Ongoing -> do
      pos <- promptPos ("\n" ++ show (gsTurn st) ++ " move (row,col): ")
      case validateAndApply st pos of
        Left err -> do
          putStrLn ("\n" ++ err)
          putStrLn "Press Enter to continue..."
          _ <- getLine
          gameLoop roundNo st
        Right st' ->
          gameLoop roundNo st'

playAgain :: Int -> IO ()
playAgain roundNo = do
  putStr "\nPlay again? (y/n): "
  ans <- getLine
  case map toLowerSafe (trim ans) of
    "y" -> roundLoop (roundNo + 1)
    "yes" -> roundLoop (roundNo + 1)
    _   -> putStrLn "Goodbye."

validateAndApply :: GameState -> (Int,Int) -> Either String GameState
validateAndApply st pos = do
  cell <- maybe (Left "Out of bounds.") Right (getCell (gsBoard st) pos)
  case cell of
    Taken _ -> Left "Cell is already taken."
    Empty -> do
      b' <- maybe (Left "Illegal move.") Right (setCell (gsBoard st) pos (gsTurn st))
      let st' = st { gsBoard = b', gsTurn = opponent (gsTurn st) }
      Right st'

toLowerSafe :: Char -> Char
toLowerSafe c
  | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise            = c

trim :: String -> String
trim = dropWhileEnd (== ' ') . dropWhile (== ' ')

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse