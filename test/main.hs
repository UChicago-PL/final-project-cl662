-- test/Main.hs
module Main (main) where

import Test.Hspec
import qualified GameRulesSpec
import qualified BoardSpec
import qualified MinimaxSpec
import qualified InputSpec

main :: IO ()
main = hspec $ do
  describe "Game rules" GameRulesSpec.spec
  describe "Board"      BoardSpec.spec
  describe "Minimax"    MinimaxSpec.spec
  describe "Input"      InputSpec.spec