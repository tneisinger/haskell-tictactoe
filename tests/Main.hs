module Main where

import qualified Data.Map as Map
import Test.Hspec

import TicTacToe.Exports

main :: IO ()
main = hspec $ do
  describe "makeSampleGS" $ do
    it "nextPlayer is Computer with inputs: O 42 []" $
      (nextPlayer <$> makeSampleGS O 42 []) `shouldBeR` Computer
    it "nextPlayer is Human with inputs: X 42 []" $
      (nextPlayer <$> makeSampleGS X 42 []) `shouldBeR` Human
    it "nextPlayer should be Human with inputs: O 2 [Cell10]" $
      (nextPlayer <$> makeSampleGS O 2 [Cell10]) `shouldBeR` Human
    it "nextPlayer should be Computer with inputs: X 2 [Cell10]" $
      (nextPlayer <$> makeSampleGS X 2 [Cell10]) `shouldBeR` Computer
    it "computerMark is X with inputs: O 42 []" $
      (computerMark <$> makeSampleGS O 42 []) `shouldBeR` X
    it "computerMark is O with inputs: X 42 []" $
      (computerMark <$> makeSampleGS X 42 []) `shouldBeR` O
    it "should equal initGameState X 13 with inputs: X 42 []" $
      makeSampleGS X 42 [] `shouldBeR` initGameState X 13
    it "should equal initGameState O 98 with inputs: O 98 []" $
      makeSampleGS O 98 [] `shouldBeR` initGameState O 98
    it "shouldn't matter if the StdGen seeds differ" $
      makeSampleGS O 98 [] `shouldBeR` initGameState O 2
    it "emptyCells should be all cells with inputs: X 11 []" $
      makeSampleGS X 11 [] `emptyCellsAre` [Cell00 ..]
    it "emptyCells should be all but Cell00 with inputs: X 11 [Cell00]" $
      makeSampleGS X 11 [Cell00] `emptyCellsAre` [Cell01 ..]
    it "emptyCells should be all but Cell11 with inputs: X 11 [Cell11]" $
      makeSampleGS X 11 [Cell11] `emptyCellsAre`
       ([Cell00 .. Cell10] ++ [Cell12 .. Cell22])
    it "Map.toList board should have [(Cell00, X), (Cell21, O)]" $
      makeSampleGS X 99 [Cell00, Cell21] `sampleBoardHas`
        [(Cell00, X), (Cell21, O)]
    it "Board should contain all listed moves" $
      let moves = [Cell00, Cell22, Cell11, Cell20, Cell21, Cell01,
                   Cell12, Cell10, Cell02]
       in makeSampleGS X 99 moves `sampleBoardHas`
        [(Cell00, X), (Cell22, O), (Cell11, X), (Cell20, O), (Cell21, X),
         (Cell01, O), (Cell12, X), (Cell10, O), (Cell02, X)]
    it "gives an error if you try to play into a full cell" $
      let moves = [Cell00, Cell22, Cell00, Cell20]
       in makeSampleGS X 99 moves `shouldSatisfy` isLeft
    it "gives an error if you try to play a move after the game is over" $
      let moves = [Cell11, Cell20, Cell00, Cell22, Cell10, Cell21, Cell02]
       in makeSampleGS X 99 moves `shouldSatisfy` isLeft

  describe "checkGSForOutcome" $ do
    it "gives Nothing if the game isn't over yet" $
      checkGSForOutcome <$> makeSampleGS X 99 [Cell00, Cell22, Cell11]
        `shouldSatisfy` (hasOutcome Nothing)
    it "gives (Just Draw) if all the cells are full and nobody won" $
      let moves = [Cell00, Cell22, Cell11, Cell02, Cell12, Cell10, Cell21,
                   Cell01, Cell20]
       in checkGSForOutcome <$> makeSampleGS X 99 moves `shouldSatisfy`
            hasOutcome (Just Draw)
    it "gives (Just (Winner Human)) if Human wins as X" $
      let moves = [Cell00, Cell22, Cell01, Cell12, Cell02]
       in checkGSForOutcome <$> makeSampleGS X 99 moves `shouldSatisfy`
            hasOutcome (Just (Winner Human))
    it "gives (Just (Winner Human)) if Human wins as O" $
      let moves = [Cell01, Cell22, Cell21, Cell11, Cell02, Cell00]
       in checkGSForOutcome <$> makeSampleGS O 99 moves `shouldSatisfy`
            hasOutcome (Just (Winner Human))
    it "gives (Just (Winner Computer)) if Computer wins as X" $
      let moves = [Cell00, Cell22, Cell01, Cell12, Cell02]
       in checkGSForOutcome <$> makeSampleGS O 99 moves `shouldSatisfy`
            hasOutcome (Just (Winner Computer))
    it "gives (Just (Winner Computer)) if Computer wins as O" $
      let moves = [Cell01, Cell22, Cell21, Cell11, Cell02, Cell00]
       in checkGSForOutcome <$> makeSampleGS X 99 moves `shouldSatisfy`
            hasOutcome (Just (Winner Computer))
    it "gives (Just (Winner Computer)) if Computer wins on full board" $
      let moves = [Cell01, Cell22, Cell21, Cell11, Cell02, Cell12, Cell10,
                   Cell20, Cell00]
       in checkGSForOutcome <$> makeSampleGS O 99 moves `shouldSatisfy`
            hasOutcome (Just (Winner Computer))
    it "gives (Just (Winner Human)) if Human wins on full board" $
      let moves = [Cell01, Cell22, Cell21, Cell11, Cell02, Cell12, Cell10,
                   Cell20, Cell00]
       in checkGSForOutcome <$> makeSampleGS X 99 moves `shouldSatisfy`
            hasOutcome (Just (Winner Human))

-- ========================================================================= --
--                              HELPER FUNCTIONS                             --
-- ========================================================================= --

-- For checkGSForOutcome --

hasOutcome :: Eq a => Maybe a -> Either b (Maybe a) -> Bool
hasOutcome _ (Left _) = False
hasOutcome mVal1 (Right mVal2) = mVal1 == mVal2

-- For makeSampleGS --

shouldBeR :: (Show a, Show b, Eq b) => Either a b -> b -> Expectation
shouldBeR (Left err) _ = expectationFailure (show err)
shouldBeR (Right a) b = a `shouldBe` b

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

shouldMatchListR :: (Show a, Show c, Eq c) =>
                 (b -> [c]) -> Either a b -> [c] -> Expectation
shouldMatchListR _ (Left err) _ = expectationFailure (show err)
shouldMatchListR f (Right x) ys = f x `shouldMatchList` ys

emptyCellsAre :: (Show a) => Either a GameState -> [Cell] -> Expectation
emptyCellsAre = shouldMatchListR (emptyCells . gameBoard)

sampleBoardHas :: (Show a)
                 => Either a GameState
                 -> [(Cell, Mark)]
                 -> Expectation
sampleBoardHas = shouldMatchListR (Map.toList . gameBoard)


