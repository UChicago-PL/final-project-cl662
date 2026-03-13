-- test/MinimaxSpec.hs
module MinimaxSpec (spec) where

import Test.Hspec

import AI.Minimax (bestMove)
import AI.SearchConfig (SearchConfig(..))
import Game.Board (emptyBoard, setCell)
import Game.State (GameState(..))
import Game.Types (Player(..))

spec :: Spec
spec = do
  describe "Minimax.bestMove (3x3x3)" $ do
    it "picks an immediate winning move" $ do
      -- P1 to move, has X X _ on top row
      -- Expected best move: (0,2)
      let b0 = emptyBoard 3 3
          b1 = must (setCell b0 (0,0) P1)
          b2 = must (setCell b1 (0,1) P1)
          st = GameState { gsBoard = b2, gsTurn = P1, gsK = 3 }
          cfg = SearchConfig { maxDepth = 2 }
      bestMove cfg st `shouldBe` Just (0,2)

    it "blocks an immediate loss when depth allows" $ do
      -- P1 to move, P2 threatens O O _ on row 1
      -- If P1 doesn't play (1,2), P2 wins next.
      let b0 = emptyBoard 3 3
          b1 = must (setCell b0 (1,0) P2)
          b2 = must (setCell b1 (1,1) P2)
          st = GameState { gsBoard = b2, gsTurn = P1, gsK = 3 }
          cfg = SearchConfig { maxDepth = 3 }
      bestMove cfg st `shouldBe` Just (1,2)

must :: Maybe a -> a
must (Just x) = x
must Nothing  = error "Unexpected Nothing in test (illegal move?)"