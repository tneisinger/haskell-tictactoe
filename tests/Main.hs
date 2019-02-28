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

  describe "doComputerMove" $ do

    -- The corners or the center are the best cells to play into as the first
    -- move of the game, so make sure that doComputerMove does that.
    it "plays into a corner or the center on first move of a game" $
      (makeSampleGS O 42 [] >>= execTicTacToe doComputerMove) `shouldBeOneOf`
       [ makeSampleGS O 11 [Cell00]
       , makeSampleGS O 88 [Cell02]
       , makeSampleGS O 19 [Cell20]
       , makeSampleGS O 55 [Cell22]
       , makeSampleGS O 38 [Cell11]
       ]

    -- If your opponent plays into a corner as the first move of the game, your
    -- best move is to play into the center.  Make sure that doComputerMove
    -- does that.
    it "If opponent starts in Cell00, play into the center cell" $
      (makeSampleGS X 9 [Cell00] >>= execTicTacToe doComputerMove)
        `shouldBe` makeSampleGS X 4 [Cell00, Cell11]
    it "If opponent starts in Cell22, play into the center cell" $
      (makeSampleGS X 9 [Cell22] >>= execTicTacToe doComputerMove)
        `shouldBe` makeSampleGS X 4 [Cell22, Cell11]
    it "If opponent starts in Cell20, play into the center cell" $
      (makeSampleGS X 9 [Cell20] >>= execTicTacToe doComputerMove)
        `shouldBe` makeSampleGS X 4 [Cell20, Cell11]
    it "If opponent starts in Cell02, play into the center cell" $
      (makeSampleGS X 9 [Cell02] >>= execTicTacToe doComputerMove)
        `shouldBe` makeSampleGS X 4 [Cell02, Cell11]

    -- Make sure that doComputerMove returns an error when it can't or
    -- shouldn't make a move.
    it "won't make a move if it is not Computer's turn" $
      (makeSampleGS X 17 [] >>= execTicTacToe doComputerMove) `shouldSatisfy`
        isLeft
    it "won't make a move if the board is full" $
      let moves = [Cell00, Cell22, Cell11, Cell02, Cell12, Cell10, Cell21,
                   Cell01, Cell20]
       in (makeSampleGS X 17 moves >>= execTicTacToe doComputerMove)
            `shouldSatisfy` isLeft
    it "won't make a move if the Human has already won the game" $
      let moves = [Cell01, Cell22, Cell21, Cell11, Cell02, Cell00]
       in (makeSampleGS O 31 moves >>= execTicTacToe doComputerMove)
            `shouldSatisfy` isLeft

    -- This sequence of moves broke the game in an earlier version
    it "makes a move even when it doesn't have a good move to make" $
      let moves = [Cell20, Cell02, Cell22, Cell21, Cell00]
       in (makeSampleGS X 99 moves >>= execTicTacToe doComputerMove)
            `shouldBeOneOf` [ makeSampleGS X 99 (moves ++ [Cell10])
                            , makeSampleGS X 99 (moves ++ [Cell11]) ]

    -- If your opponent starts the game by playing in the center,
    -- always play into a corner as your first move.
    it "plays into a corner cell if opponent plays first move in center" $
      (makeSampleGS X 99 [Cell11] >>= execTicTacToe doComputerMove)
        `shouldBeOneOf` [ makeSampleGS X 99 [Cell11, Cell00]
                        , makeSampleGS X 99 [Cell11, Cell02]
                        , makeSampleGS X 99 [Cell11, Cell20]
                        , makeSampleGS X 99 [Cell11, Cell22]
                        ]

-- ========================================================================= --
--                              HELPER FUNCTIONS                             --
-- ========================================================================= --

-- General

shouldBeR :: (Show a, Show b, Eq b) => Either a b -> b -> Expectation
shouldBeR (Left err) _ = expectationFailure (show err)
shouldBeR (Right a) b = a `shouldBe` b

shouldBeOneOf :: Eq a => a -> [a] -> Expectation
shouldBeOneOf x xs =
  if any (== x) xs
     then pure ()
     else expectationFailure "element not found in list by shouldBeOneOf"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False


-- For checkGSForOutcome

hasOutcome :: Eq a => Maybe a -> Either b (Maybe a) -> Bool
hasOutcome _ (Left _) = False
hasOutcome mVal1 (Right mVal2) = mVal1 == mVal2


-- For makeSampleGS

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
