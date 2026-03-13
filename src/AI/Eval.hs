module AI.Eval
  ( evalState
  ) where

import Game.Board (Board, getCell)
import Game.Rules (Outcome(..), checkOutcome, linesOfLengthK)
import Game.State (GameState(..))
import Game.Types (Cell(..), Player(..), opponent)

evalState :: Player -> GameState -> Int
evalState me st =
  case checkOutcome (gsK st) (gsBoard st) of
    Winner p
      | p == me   -> 100000
      | otherwise -> -100000
    Draw -> 0
    Ongoing ->
      lineHeuristic me st

lineHeuristic :: Player -> GameState -> Int
lineHeuristic me st =
  let b = gsBoard st
      k = gsK st
      segs = linesOfLengthK k b
  in sum (map (scoreSeg me b) segs)

scoreSeg :: Player -> (Game.Board.Board) -> [(Int,Int)] -> Int
scoreSeg me b seg =
  case traverse (getCell b) seg of
    Nothing -> 0
    Just cells ->
      let mine = countTaken me cells
          theirs = countTaken (opponent me) cells
          empties = countEmpty cells
      in if mine > 0 && theirs > 0
           then 0
           else if mine > 0
             then pow 10 mine
             else if theirs > 0
               then negate (pow 10 theirs)
               else 0

countTaken :: Player -> [Cell] -> Int
countTaken p = length . filter (== Taken p)

countEmpty :: [Cell] -> Int
countEmpty = length . filter (== Empty)

pow :: Int -> Int -> Int
pow base expn = base ^ expn