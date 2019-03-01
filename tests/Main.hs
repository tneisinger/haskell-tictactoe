module Main where

import qualified Data.Map as Map
import Test.Hspec
import Test.QuickCheck

import TicTacToe.Exports

main :: IO ()
main = hspec $ do
  -- The makeSampleGS function takes an Int as its second argument.  That
  -- Int argument is used to create a StdGen so that the AI can randomly
  -- select moves.  The StdGen is stored in the resulting GameState.  When
  -- testing functions that require an Int for the creation of the StdGen, we
  -- use QuickCheck to generate an arbitrary (i.e. random) Int.  Where
  -- 'n' is used in test descriptions, that should be understood to represent
  -- an arbitrary Int value.
  describe "makeSampleGS" $ do
    it "nextPlayer is Computer with inputs: O n []" $
      forAll (arbitrary :: Gen Int) $ \n ->
        (nextPlayer <$> makeSampleGS O n []) `shouldBeR` Computer
    it "nextPlayer is Human with inputs: X n []" $
      forAll (arbitrary :: Gen Int) $ \n ->
        (nextPlayer <$> makeSampleGS X n []) `shouldBeR` Human
    it "nextPlayer should be Human with inputs: O n [Cell10]" $
      forAll (arbitrary :: Gen Int) $ \n ->
        (nextPlayer <$> makeSampleGS O n [Cell10]) `shouldBeR` Human
    it "nextPlayer should be Computer with inputs: X n [Cell10]" $
      forAll (arbitrary :: Gen Int) $ \n ->
        (nextPlayer <$> makeSampleGS X n [Cell10]) `shouldBeR` Computer
    it "computerMark is X with inputs: O n []" $
      forAll (arbitrary :: Gen Int) $ \n ->
        (computerMark <$> makeSampleGS O n []) `shouldBeR` X
    it "computerMark is O with inputs: X n []" $
      forAll (arbitrary :: Gen Int) $ \n ->
        (computerMark <$> makeSampleGS X n []) `shouldBeR` O
    it "should equal initGameState X n2 with inputs: X n1 []" $
      forAll (arbitrary :: Gen (Int, Int)) $ \(n1, n2) ->
        makeSampleGS X n1 [] `shouldBeR` initGameState X n2
    it "should equal initGameState O n2 with inputs: O n1 []" $
      forAll (arbitrary :: Gen (Int, Int)) $ \(n1, n2) ->
        makeSampleGS O n1 [] `shouldBeR` initGameState O n2
    it "shouldn't matter if the StdGen seeds differ" $
      forAll (arbitrary :: Gen (Int, Int)) $ \(n1, n2) ->
        makeSampleGS O n1 [] `shouldBeR` initGameState O n2
    it "emptyCells should be all cells with inputs: X n []" $
      forAll (arbitrary :: Gen Int) $ \n ->
        makeSampleGS X n [] `emptyCellsAre` [Cell00 ..]
    it "emptyCells should be all but Cell00 with inputs: X n [Cell00]" $
      forAll (arbitrary :: Gen Int) $ \n ->
        makeSampleGS X n [Cell00] `emptyCellsAre` [Cell01 ..]
    it "emptyCells should be all but Cell11 with inputs: X n [Cell11]" $
      forAll (arbitrary :: Gen Int) $ \n ->
        makeSampleGS X n [Cell11] `emptyCellsAre`
         ([Cell00 .. Cell10] ++ [Cell12 .. Cell22])
    it "Map.toList board should have [(Cell00, X), (Cell21, O)]" $
      forAll (arbitrary :: Gen Int) $ \n ->
        makeSampleGS X n [Cell00, Cell21] `sampleBoardHas`
          [(Cell00, X), (Cell21, O)]
    it "Board should contain all listed moves" $
      let moves = [Cell00, Cell22, Cell11, Cell20, Cell21, Cell01,
                   Cell12, Cell10, Cell02]
          res = [(Cell00, X), (Cell22, O), (Cell11, X), (Cell20, O),
                 (Cell21, X), (Cell01, O), (Cell12, X), (Cell10, O),
                 (Cell02, X)]
       in forAll (arbitrary :: Gen Int) $ \n ->
         makeSampleGS X n moves `sampleBoardHas` res
    it "gives an error if you try to play into a full cell" $
      let moves = [Cell00, Cell22, Cell00, Cell20]
       in forAll (arbitrary :: Gen Int) $ \n ->
         makeSampleGS X n moves `shouldSatisfy` isLeft
    it "gives an error if you try to play a move after the game is over" $
      let moves = [Cell11, Cell20, Cell00, Cell22, Cell10, Cell21, Cell02]
       in forAll (arbitrary :: Gen Int) $ \n ->
         makeSampleGS X n moves `shouldSatisfy` isLeft

  describe "checkGSForOutcome" $ do
    it "gives Nothing if the game isn't over yet" $
      forAll (arbitrary :: Gen Int) $ \n ->
        checkGSForOutcome <$> makeSampleGS X n [Cell00, Cell22, Cell11]
          `shouldSatisfy` hasOutcome Nothing
    it "gives (Just Draw) if all the cells are full and nobody won" $
      let moves = [Cell00, Cell22, Cell11, Cell02, Cell12, Cell10, Cell21,
                   Cell01, Cell20]
       in forAll (arbitrary :: Gen Int) $ \n ->
         checkGSForOutcome <$> makeSampleGS X n moves `shouldSatisfy`
            hasOutcome (Just Draw)
    it "gives (Just (Winner Human)) if Human wins as X" $
      let moves = [Cell00, Cell22, Cell01, Cell12, Cell02]
       in forAll (arbitrary :: Gen Int) $ \n ->
         checkGSForOutcome <$> makeSampleGS X n moves `shouldSatisfy`
            hasOutcome (Just (Winner Human))
    it "gives (Just (Winner Human)) if Human wins as O" $
      let moves = [Cell01, Cell22, Cell21, Cell11, Cell02, Cell00]
       in forAll (arbitrary :: Gen Int) $ \n ->
         checkGSForOutcome <$> makeSampleGS O n moves `shouldSatisfy`
            hasOutcome (Just (Winner Human))
    it "gives (Just (Winner Computer)) if Computer wins as X" $
      let moves = [Cell00, Cell22, Cell01, Cell12, Cell02]
       in forAll (arbitrary :: Gen Int) $ \n ->
         checkGSForOutcome <$> makeSampleGS O n moves `shouldSatisfy`
            hasOutcome (Just (Winner Computer))
    it "gives (Just (Winner Computer)) if Computer wins as O" $
      let moves = [Cell01, Cell22, Cell21, Cell11, Cell02, Cell00]
       in forAll (arbitrary :: Gen Int) $ \n ->
         checkGSForOutcome <$> makeSampleGS X n moves `shouldSatisfy`
           hasOutcome (Just (Winner Computer))
    it "gives (Just (Winner Computer)) if Computer wins on full board" $
      let moves = [Cell01, Cell22, Cell21, Cell11, Cell02, Cell12, Cell10,
                   Cell20, Cell00]
       in forAll (arbitrary :: Gen Int) $ \n ->
         checkGSForOutcome <$> makeSampleGS O n moves `shouldSatisfy`
           hasOutcome (Just (Winner Computer))
    it "gives (Just (Winner Human)) if Human wins on full board" $
      let moves = [Cell01, Cell22, Cell21, Cell11, Cell02, Cell12, Cell10,
                   Cell20, Cell00]
       in forAll (arbitrary :: Gen Int) $ \n ->
         checkGSForOutcome <$> makeSampleGS X n moves `shouldSatisfy`
           hasOutcome (Just (Winner Human))

  describe "doComputerMove" $ do
    -- The corners or the center are the best cells to play into as the first
    -- move of the game, so make sure that doComputerMove does that.  This test
    -- ensures that all the corners and the center do in fact get played into
    -- as the first move in some cases.  Since the selection of the first move
    -- should be a random selection of the corners and the center, we need to
    -- make sure that each of those do actually get played some of the time.
    it "plays into the corners or the center on first move of a game" $ do
      ints <- generate $ resize 200 $ vector 200
      let f n = makeSampleGS O n [] >>= execTicTacToe doComputerMove
          gameStates = rightsOnly $ f <$> ints
          getCells gs = Map.keys (gameBoard gs)
      concat (getCells <$> gameStates) `containsEachOf`
        [Cell00, Cell02, Cell20, Cell22, Cell11]

    -- The edges are the worst cells to play into as the first move of the
    -- game, so make sure that doComputerMove doesn't do that.
    it "does not play into an edge in the first move of a game" $
      forAll (arbitrary :: Gen (Int, Int, Int, Int, Int)) $
        \(n1, n2, n3, n4, n5) ->
          (makeSampleGS O n1 [] >>= execTicTacToe doComputerMove)
            `shouldBeNoneOf` [ makeSampleGS O n2 [Cell01]
                             , makeSampleGS O n3 [Cell10]
                             , makeSampleGS O n4 [Cell12]
                             , makeSampleGS O n5 [Cell21]
                             ]

    -- If your opponent plays into a corner as the first move of the game, your
    -- best move is to play into the center.  Make sure that doComputerMove
    -- does that.
    it "If opponent starts in Cell00, play into the center cell" $
      forAll (arbitrary :: Gen (Int, Int)) $ \(n1, n2) ->
        (makeSampleGS X n1 [Cell00] >>= execTicTacToe doComputerMove)
          `shouldBe` makeSampleGS X n2 [Cell00, Cell11]
    it "If opponent starts in Cell22, play into the center cell" $
      forAll (arbitrary :: Gen (Int, Int)) $ \(n1, n2) ->
        (makeSampleGS X n1 [Cell22] >>= execTicTacToe doComputerMove)
          `shouldBe` makeSampleGS X n2 [Cell22, Cell11]
    it "If opponent starts in Cell20, play into the center cell" $
      forAll (arbitrary :: Gen (Int, Int)) $ \(n1, n2) ->
        (makeSampleGS X n1 [Cell20] >>= execTicTacToe doComputerMove)
          `shouldBe` makeSampleGS X n2 [Cell20, Cell11]
    it "If opponent starts in Cell02, play into the center cell" $
      forAll (arbitrary :: Gen (Int, Int)) $ \(n1, n2) ->
        (makeSampleGS X n1 [Cell02] >>= execTicTacToe doComputerMove)
          `shouldBe` makeSampleGS X n2 [Cell02, Cell11]

    -- Make sure that doComputerMove returns an error when it can't or
    -- shouldn't make a move.
    it "won't make a move if it is not Computer's turn" $
      forAll (arbitrary :: Gen Int) $ \n ->
        (makeSampleGS X n [] >>= execTicTacToe doComputerMove) `shouldSatisfy`
          isLeft
    it "won't make a move if the board is full" $
      let moves = [Cell00, Cell22, Cell11, Cell02, Cell12, Cell10, Cell21,
                   Cell01, Cell20]
       in forAll (arbitrary :: Gen Int) $ \n ->
         (makeSampleGS X n moves >>= execTicTacToe doComputerMove)
           `shouldSatisfy` isLeft
    it "won't make a move if the Human has already won the game" $
      let moves = [Cell01, Cell22, Cell21, Cell11, Cell02, Cell00]
       in forAll (arbitrary :: Gen Int) $ \n ->
         (makeSampleGS O n moves >>= execTicTacToe doComputerMove)
           `shouldSatisfy` isLeft

    -- This sequence of moves broke the game in an earlier version, so
    -- make sure that that doesn't happen again.
    it "makes a move even when it doesn't have a good move to make" $
      let moves = [Cell20, Cell02, Cell22, Cell21, Cell00]
       in forAll (arbitrary :: Gen (Int, Int, Int)) $ \(n1, n2, n3) ->
         (makeSampleGS X n1 moves >>= execTicTacToe doComputerMove)
            `shouldBeOneOf` [ makeSampleGS X n2 (moves ++ [Cell10])
                            , makeSampleGS X n3 (moves ++ [Cell11]) ]

    -- If your opponent starts the game by playing in the center,
    -- always play into a corner as your first move.
    it "plays into a corner cell if opponent plays first move in center" $
      forAll (arbitrary :: Gen (Int, Int, Int, Int, Int)) $
        \(n1, n2, n3, n4, n5) ->
          (makeSampleGS X n1 [Cell11] >>= execTicTacToe doComputerMove)
            `shouldBeOneOf` [ makeSampleGS X n2 [Cell11, Cell00]
                            , makeSampleGS X n3 [Cell11, Cell02]
                            , makeSampleGS X n4 [Cell11, Cell20]
                            , makeSampleGS X n5 [Cell11, Cell22]
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
  if x `elem` xs
     then pure ()
     else expectationFailure "element not found in list by shouldBeOneOf"

shouldBeNoneOf :: Eq a => a -> [a] -> Expectation
shouldBeNoneOf x xs =
  if x `elem` xs
     then expectationFailure "element not found in list by shouldBeOneOf"
     else pure ()

containsEachOf :: Eq a => [a] -> [a] -> Expectation
containsEachOf longList shortList =
  if all (`elem` longList) shortList
     then pure ()
     else expectationFailure "not all elems of shortList found: containsEachOf"

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft (Right _) = False

rightsOnly :: [Either a b] -> [b]
rightsOnly = foldr go []
  where go (Right b) result = b:result
        go (Left _) result = result


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
