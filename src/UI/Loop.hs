-- src/UI/Loop.hs
module UI.Loop
  ( run
  ) where

import System.Environment (getArgs)
import System.IO (hFlush, stdout)

import AI.Minimax (bestMove)
import AI.SearchConfig (defaultConfig)

import Game.Board (getCell)
import Game.Rules (Outcome(..), checkOutcome)
import Game.State (GameState(..), initState, applyMove)
import Game.Types (Cell(..), Player(..))

import UI.Ansi (clearScreenSafe)
import UI.Input (promptPos, promptMNK, parseMNK)
import UI.Render (render)

data Mode = HvH | HvBot deriving (Eq, Show)

run :: IO ()
run = do
  mnk <- chooseMNK
  mode <- chooseMode
  roundLoop 1 mode mnk

chooseMNK :: IO (Int, Int, Int)
chooseMNK = do
  args <- getArgs
  case args of
    [a,b,c] ->
      case parseMNK (unwords [a,b,c]) of
        Right mnk -> pure mnk
        Left err  -> do
          putStrLn ("Bad CLI args: " ++ err)
          promptMNK (3,3,3)
    _ -> promptMNK (3,3,3)

chooseMode :: IO Mode
chooseMode = do
  putStrLn "Mode: 1) Human vs Human   2) Human vs Bot (Bot as P2)"
  putStr "Select (1/2) [default 1]: "
  hFlush stdout
  s <- getLine
  case trimSpaces s of
    "2" -> pure HvBot
    _   -> pure HvH

roundLoop :: Int -> Mode -> (Int, Int, Int) -> IO ()
roundLoop roundNo mode (m,n,k) = do
  let st0 = initState m n k
  gameLoop roundNo mode (m,n,k) st0

gameLoop :: Int -> Mode -> (Int, Int, Int) -> GameState -> IO ()
gameLoop roundNo mode mnk@(m,n,k) st = do
  clearScreenSafe
  putStrLn ("mnk-game  |  Round " ++ show roundNo
         ++ "  |  " ++ show m ++ "x" ++ show n ++ " k=" ++ show k
         ++ "  |  " ++ show mode)
  putStrLn ""
  putStrLn (render st)

  case checkOutcome (gsK st) (gsBoard st) of
    Winner p -> do
      putStrLn ("\nResult: " ++ show p ++ " wins.")
      postGameMenu roundNo mode mnk
    Draw -> do
      putStrLn "\nResult: Draw."
      postGameMenu roundNo mode mnk
    Ongoing ->
      case mode of
        HvH  -> humanTurn
        HvBot -> if gsTurn st == P2 then aiTurn else humanTurn
  where
    humanTurn :: IO ()
    humanTurn = do
      pos <- promptPos ("\n" ++ show (gsTurn st) ++ " move (row,col): ")
      case validateAndApply st pos of
        Left err -> do
          putStrLn ("\n" ++ err)
          putStrLn "Press Enter to continue..."
          _ <- getLine
          gameLoop roundNo mode mnk st
        Right st' ->
          gameLoop roundNo mode mnk st'

    aiTurn :: IO ()
    aiTurn = do
      putStrLn "\nBot thinking..."
      case bestMove defaultConfig st of
        Nothing -> do
          putStrLn "Bot: no legal moves."
          putStrLn "Press Enter to continue..."
          _ <- getLine
          gameLoop roundNo mode mnk st
        Just mv ->
          case applyMove mv st of
            Nothing  -> error "Bot produced illegal move (bug)"
            Just st' -> gameLoop roundNo mode mnk st'

postGameMenu :: Int -> Mode -> (Int, Int, Int) -> IO ()
postGameMenu roundNo mode mnk = do
  putStr "\nPlay again? [y = same settings, c = change settings, n = quit]: "
  hFlush stdout
  ans <- getLine
  case map toLowerSafe (trimSpaces ans) of
    "y"   -> roundLoop (roundNo + 1) mode mnk
    "yes" -> roundLoop (roundNo + 1) mode mnk
    "c"   -> do
      mnk' <- promptMNK (3,3,3)
      mode' <- chooseMode
      roundLoop (roundNo + 1) mode' mnk'
    "change" -> do
      mnk' <- promptMNK (3,3,3)
      mode' <- chooseMode
      roundLoop (roundNo + 1) mode' mnk'
    "n"   -> putStrLn "Goodbye."
    "no"  -> putStrLn "Goodbye."
    _     -> do
      putStrLn "Please enter y, c, or n."
      postGameMenu roundNo mode mnk

validateAndApply :: GameState -> (Int, Int) -> Either String GameState
validateAndApply st pos = do
  cell <- maybe (Left "Out of bounds.") Right (getCell (gsBoard st) pos)
  case cell of
    Taken _ -> Left "Cell is already taken."
    Empty ->
      case applyMove pos st of
        Nothing  -> Left "Illegal move."
        Just st' -> Right st'

toLowerSafe :: Char -> Char
toLowerSafe c
  | 'A' <= c && c <= 'Z' = toEnum (fromEnum c + 32)
  | otherwise            = c

trimSpaces :: String -> String
trimSpaces = dropWhileEnd (== ' ') . dropWhile (== ' ')

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p = reverse . dropWhile p . reverse