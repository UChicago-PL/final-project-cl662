module AI.Minimax
  ( bestMove
  ) where

import Control.Monad.State.Strict (State, get, modify)
import qualified Control.Monad.State.Strict as S
import qualified Data.Set as Set

import qualified AI.Eval as Eval
import AI.SearchConfig (SearchConfig(..))

import qualified Game.Board as Board
import Game.Rules (Outcome(..), checkOutcome)
import Game.State (GameState(..), applyMove)
import Game.Types (Cell(..), Player, Pos)

data SearchEnv = SearchEnv
  { seCfg   :: SearchConfig
  , seNodes :: Int
  }

bestMove :: SearchConfig -> GameState -> Maybe Pos
bestMove cfg st =
  case candidateMoves cfg st of
    [] -> Nothing
    ms ->
      let me     = gsTurn st
          env0   = SearchEnv cfg 0
          scored = S.evalState (mapM (scoreMove me st) ms) env0
      in Just (fst (argmax snd scored))

scoreMove :: Player -> GameState -> Pos -> State SearchEnv (Pos, Int)
scoreMove me st mv =
  case applyMove mv st of
    Nothing  -> pure (mv, negInf)
    Just st' -> do
      cfg <- getsCfg
      let d = max 0 (maxDepth cfg - 1)
      v <- if useAlphaBeta cfg
            then alphabeta cfg d me st' False (negInf, posInf)
            else minimax   cfg d me st' False
      pure (mv, v)

-- Candidate move generation: neighbor-only
candidateMoves :: SearchConfig -> GameState -> [Pos]
candidateMoves cfg st =
  let b = gsBoard st
      (rows, cols) = Board.boardSize b
      r = max 0 (neighborRadius cfg)
      occ = occupiedPositions b rows cols
      empties = emptyPositions b rows cols
  in case occ of
      [] -> centerCandidates rows cols
      _  ->
        let neigh = neighborEmptySet rows cols r occ empties
            ms = Set.toList neigh
        in if null ms then empties else ms

occupiedPositions :: Board.Board -> Int -> Int -> [Pos]
occupiedPositions b rows cols =
  [ (i,j)
  | i <- [0..rows-1]
  , j <- [0..cols-1]
  , Board.getCell b (i,j) /= Just Empty
  ]

emptyPositions :: Board.Board -> Int -> Int -> [Pos]
emptyPositions b rows cols =
  [ (i,j)
  | i <- [0..rows-1]
  , j <- [0..cols-1]
  , Board.getCell b (i,j) == Just Empty
  ]

neighborEmptySet :: Int -> Int -> Int -> [Pos] -> [Pos] -> Set.Set Pos
neighborEmptySet rows cols r occ empties =
  let emptySet = Set.fromList empties
      inBounds (i,j) = i >= 0 && i < rows && j >= 0 && j < cols
      neighbors (i,j) =
        [ (i+di, j+dj)
        | di <- [-r..r]
        , dj <- [-r..r]
        , let p = (i+di, j+dj)
        , inBounds p
        ]
      allNeigh = Set.fromList (concatMap neighbors occ)
  in Set.intersection allNeigh emptySet

centerCandidates :: Int -> Int -> [Pos]
centerCandidates rows cols =
  let cr1 = (rows - 1) `div` 2
      cr2 = rows `div` 2
      cc1 = (cols - 1) `div` 2
      cc2 = cols `div` 2
      ps  = Set.toList (Set.fromList [(cr1,cc1),(cr1,cc2),(cr2,cc1),(cr2,cc2)])
  in ps

-- Minimax Algorithm

minimax :: SearchConfig -> Int -> Player -> GameState -> Bool -> State SearchEnv Int
minimax cfg depth me st maximizing = do
  mTerm <- terminalMaybe cfg depth me st
  case mTerm of
    Just v  -> pure v
    Nothing -> do
      ok <- bumpNodeAndCheck cfg
      if not ok
        then pure (Eval.evalState me st)
        else do
          moves0 <- orderedMoves cfg me st maximizing
          vals <- mapM (\m -> case applyMove m st of
                                Nothing  -> pure (if maximizing then negInf else posInf)
                                Just st' -> minimax cfg (depth - 1) me st' (not maximizing)
                       ) moves0
          pure $ if maximizing then maximum vals else minimum vals

alphabeta :: SearchConfig -> Int -> Player -> GameState -> Bool -> (Int, Int) -> State SearchEnv Int
alphabeta cfg depth me st maximizing (alpha0, beta0) = do
  mTerm <- terminalMaybe cfg depth me st
  case mTerm of
    Just v  -> pure v
    Nothing -> do
      moves0 <- orderedMoves cfg me st maximizing
      go moves0 alpha0 beta0 (if maximizing then negInf else posInf)
  where
    go :: [Pos] -> Int -> Int -> Int -> State SearchEnv Int
    go [] _a _b best = pure best
    go (m:ms) a b best = do
      ok <- bumpNodeAndCheck cfg
      if not ok
        then pure best
        else case applyMove m st of
          Nothing -> go ms a b best
          Just st' -> do
            child <- alphabeta cfg (depth - 1) me st' (not maximizing) (a, b)
            let best' = if maximizing then max best child else min best child
            let a'    = if maximizing then max a best' else a
            let b'    = if maximizing then b else min b best'
            if a' >= b'
              then pure best'
              else go ms a' b' best'

terminalMaybe :: SearchConfig -> Int -> Player -> GameState -> State SearchEnv (Maybe Int)
terminalMaybe cfg depth me st = do
  exceeded <- nodeLimitExceeded cfg
  if exceeded
    then pure (Just (Eval.evalState me st))
    else
      case checkOutcome (gsK st) (gsBoard st) of
        Winner _ -> pure (Just (Eval.evalState me st))
        Draw     -> pure (Just 0)
        Ongoing
          | depth <= 0 -> pure (Just (Eval.evalState me st))
          | null (candidateMoves cfg st) -> pure (Just (Eval.evalState me st))
          | otherwise -> pure Nothing

-- Move ordering

orderedMoves :: SearchConfig -> Player -> GameState -> Bool -> State SearchEnv [Pos]
orderedMoves cfg me st maximizing = do
  let ms = candidateMoves cfg st
  if not (useMoveOrdering cfg) || length ms <= 1
    then pure ms
    else do
      scored <- mapM (\m -> case applyMove m st of
                              Nothing  -> pure (m, if maximizing then negInf else posInf)
                              Just st' -> pure (m, Eval.evalState me st')
                    ) ms
      pure (map fst (sortByScore maximizing scored))

sortByScore :: Bool -> [(a,Int)] -> [(a,Int)]
sortByScore maximizing xs =
  let cmp    (_,v1) (_,v2) = compare v2 v1
      cmpMin (_,v1) (_,v2) = compare v1 v2
  in if maximizing then sortBy cmp xs else sortBy cmpMin xs

sortBy :: (a -> a -> Ordering) -> [a] -> [a]
sortBy _ [] = []
sortBy cmp (x:xs) = sortBy cmp lesser ++ [x] ++ sortBy cmp greater
  where
    lesser  = [y | y <- xs, cmp y x /= GT]
    greater = [y | y <- xs, cmp y x == GT]


-- Node accounting

getsCfg :: State SearchEnv SearchConfig
getsCfg = do
  env <- get
  pure (seCfg env)

bumpNodeAndCheck :: SearchConfig -> State SearchEnv Bool
bumpNodeAndCheck cfg = do
  modify (\e -> e { seNodes = seNodes e + 1 })
  exceeded <- nodeLimitExceeded cfg
  pure (not exceeded)

nodeLimitExceeded :: SearchConfig -> State SearchEnv Bool
nodeLimitExceeded cfg =
  case nodeLimit cfg of
    Nothing -> pure False
    Just lim -> do
      env <- get
      pure (seNodes env >= lim)

argmax :: Ord b => (a -> b) -> [a] -> a
argmax f (x:xs) = go x xs
  where
    go best [] = best
    go best (y:ys)
      | f y > f best = go y ys
      | otherwise    = go best ys
argmax _ [] = error "argmax: empty list"

negInf, posInf :: Int
negInf = -1000000000
posInf =  1000000000