-- test/GameRulesSpec.hs
module GameRulesSpec (spec) where

import Test.Hspec

import Game.Board (emptyBoard, setCell)
import Game.Rules (Outcome(..), checkOutcome)
import Game.Types (Player(..))

spec :: Spec
spec = do
  describe "checkOutcome (3x3 k=3)" $ do
    it "detects a horizontal win" $ do
      let b0 = emptyBoard 3 3
          b1 = must (setCell b0 (0,0) P1)
          b2 = must (setCell b1 (0,1) P1)
          b3 = must (setCell b2 (0,2) P1)
      checkOutcome 3 b3 `shouldBe` Winner P1

    it "detects a vertical win" $ do
      let b0 = emptyBoard 3 3
          b1 = must (setCell b0 (0,1) P2)
          b2 = must (setCell b1 (1,1) P2)
          b3 = must (setCell b2 (2,1) P2)
      checkOutcome 3 b3 `shouldBe` Winner P2

    it "detects a diagonal down-right win" $ do
      let b0 = emptyBoard 3 3
          b1 = must (setCell b0 (0,0) P1)
          b2 = must (setCell b1 (1,1) P1)
          b3 = must (setCell b2 (2,2) P1)
      checkOutcome 3 b3 `shouldBe` Winner P1

    it "detects a diagonal down-left win" $ do
      let b0 = emptyBoard 3 3
          b1 = must (setCell b0 (0,2) P2)
          b2 = must (setCell b1 (1,1) P2)
          b3 = must (setCell b2 (2,0) P2)
      checkOutcome 3 b3 `shouldBe` Winner P2

    it "does not report a win when marks are non-contiguous" $ do
      let b0 = emptyBoard 3 3
          b1 = must (setCell b0 (0,0) P1)
          b2 = must (setCell b1 (0,2) P1)
      checkOutcome 3 b2 `shouldBe` Ongoing

    it "detects a draw on a full board with no winner" $ do
      -- Classic draw position (no 3-in-a-row):
      -- X O X
      -- X X O
      -- O X O
      let b0 = emptyBoard 3 3
          b1 = must (setCell b0 (0,0) P1)
          b2 = must (setCell b1 (0,1) P2)
          b3 = must (setCell b2 (0,2) P1)
          b4 = must (setCell b3 (1,0) P1)
          b5 = must (setCell b4 (1,1) P1)
          b6 = must (setCell b5 (1,2) P2)
          b7 = must (setCell b6 (2,0) P2)
          b8 = must (setCell b7 (2,1) P1)
          b9 = must (setCell b8 (2,2) P2)
      checkOutcome 3 b9 `shouldBe` Draw

  describe "checkOutcome (5x5 k=4)" $ do
    it "detects a horizontal win of length 4" $ do
      let b0 = emptyBoard 5 5
          b1 = must (setCell b0 (2,0) P1)
          b2 = must (setCell b1 (2,1) P1)
          b3 = must (setCell b2 (2,2) P1)
          b4 = must (setCell b3 (2,3) P1)
      checkOutcome 4 b4 `shouldBe` Winner P1

    it "detects a diagonal win of length 4" $ do
      let b0 = emptyBoard 5 5
          b1 = must (setCell b0 (0,1) P2)
          b2 = must (setCell b1 (1,2) P2)
          b3 = must (setCell b2 (2,3) P2)
          b4 = must (setCell b3 (3,4) P2)
      checkOutcome 4 b4 `shouldBe` Winner P2

    it "does not falsely report a win with only 3-in-a-row when k=4" $ do
      let b0 = emptyBoard 5 5
          b1 = must (setCell b0 (4,0) P1)
          b2 = must (setCell b1 (4,1) P1)
          b3 = must (setCell b2 (4,2) P1)
      checkOutcome 4 b3 `shouldBe` Ongoing

must :: Maybe a -> a
must (Just x) = x
must Nothing  = error "Unexpected Nothing in test (illegal move?)"