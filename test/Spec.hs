module Spec
  ( main
  ) where

import Game
import Test.Hspec

gameDraw, gameComputerWins, gamePlayerWins :: Game
gameDraw =
  Game
    0
    User
    3
    Nothing
    [Cell 1 1, Cell 0 0, Cell 1 2, Cell 2 1, Cell 2 0]
    [Cell 0 1, Cell 0 2, Cell 1 0, Cell 2 2]

gamePlayerWins =
  Game
    0
    Computer
    3
    Nothing
    [Cell 1 1, Cell 0 0, Cell 2 0]
    [Cell 0 2, Cell 1 2, Cell 2 2]

gameComputerWins =
  Game
    0
    Computer
    3
    Nothing
    [Cell 2 2, Cell 0 0, Cell 1 2, Cell 2 1, Cell 2 0]
    [Cell 0 1, Cell 0 2, Cell 1 0, Cell 1 1]

checkWinnerTest :: SpecWith ()
checkWinnerTest =
  describe "check who won" $ do
    it "check for draw" $ getWinner gameDraw `shouldBe` Nothing
    it "check computer wins" $
      getWinner gameComputerWins `shouldBe` Just Computer
    it "check player wins" $ getWinner gamePlayerWins `shouldBe` Just User

main :: IO ()
main = hspec $ do checkWinnerTest
