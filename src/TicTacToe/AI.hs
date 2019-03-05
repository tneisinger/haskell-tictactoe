module TicTacToe.AI
       ( doComputerMove
       ) where

import Control.Monad.Except (throwError)
import Control.Monad.State (execStateT, get, put, lift)
import Data.Function (on)
import Data.List (maximumBy)
import Data.Map (Map)
import Data.Maybe (isJust)
import Data.Tree (flatten, Tree(..))
import Numeric.MathFunctions.Comparison (eqRelErr)
import System.Random (randomR)

import TicTacToe.Basic (checkForOutcome, flipPlayer, flipMark, nextMark,
                        getAllLineMarks, emptyCells, performMove)
import TicTacToe.Types (Board, Cell, Player(..), GameState(..), Mark,
                        GameOutcome(..), MoveError, Cell(..), TicTacToe,
                        MoveError(..))

import qualified Data.Map as Map


{-| Randomly select from among the best moves available, and make that move.

This function will throw a MoveError if it is not the Computer's turn to play.
-}
doComputerMove :: TicTacToe ()
doComputerMove = do
  gs <- get
  case nextPlayer gs of
    Human -> lift $ throwError (NotTurnOf Computer)
    Computer -> do
      cell <- getRandomBestCell
      performMove cell

{-| Get a random Int in the TicTacToe monad.  The StateT part of the TicTacToe
monad uses GameState as its State value.  All GameStates contain a StdGen,
which is used to generate the random number in this function.
-}
getRandomRFromState :: (Int, Int) -> TicTacToe Int
getRandomRFromState (low,hi) = do
  gs <- get
  let (randomInt, gen') = randomR (low,hi) (stdGen gs)
  put $ gs { stdGen = gen' }
  pure randomInt

{-| Get a random element from a list when working in the TicTacToe monad. The
randomness is generated from the StdGen of a GameState.  See the definitions
of GameState and of TicTacToe for details.
-}
getRandomElement :: [a] -> TicTacToe a
getRandomElement xs = do
  randomInt <- getRandomRFromState (0, length xs - 1)
  pure $ xs !! randomInt

{-| Based on the current GameState, return a random Cell from among the Cells
that would be the best move for the Computer Player.

This function uses the getBestCells function and the getBestCellsEarlyGame
function.  The getBestCells function does the work of calculating the best
moves for the computer to make based on the given GameState, which can be
pretty costly when used on a GameState in which the board has only zero or one
marks on it.

For GameStates that have zero or one marks on the board, the
getBestCellsEarlyGame function is used instead. For boards that have zero or
one marks, the getBestCellsEarlyGame function will return the same result as
the getBestCells function, but without calculating.  This avoids the heavy
calculations.

If there is more than one mark on the board, getBestCellsEarlyGame will return
an empty list.  In that case, use the getBestCells function to calculate the
best moves to make.
-}
getRandomBestCell :: TicTacToe Cell
getRandomBestCell = do
  gs <- get
  case getBestCellsEarlyGame gs of
    []        -> lift (getBestCells gs) >>= getRandomElement
    bestCells -> getRandomElement bestCells

data CellType = Center | Corner | Edge
  deriving (Eq, Show)

cellTypeToCells :: CellType -> [Cell]
cellTypeToCells Center = [Cell11]
cellTypeToCells Corner = [Cell00, Cell02, Cell20, Cell22]
cellTypeToCells Edge   = [Cell01, Cell10, Cell12, Cell21]

cellToCellType :: Cell -> CellType
cellToCellType Cell11 = Center
cellToCellType cell = if cell `elem` cellTypeToCells Corner
                         then Corner
                         else Edge

{-| If only zero or one marks are on the board and it is
the Computer's turn, return the best cells for the Computer to play into. If it
is not the Computer's turn, or if there is more than one mark on the board,
return an empty list. Instead of calculating the best cells, just return
hard-coded values.

In order to save on calculation time, this function "hard-codes" the best cells
to play into based on the given GameState.

In the case where no moves are returned by this function, you should calculate
the best moves with the getBestCells function instead, as in the
getRandomBestCell function.
-}
getBestCellsEarlyGame :: GameState -> [Cell]
getBestCellsEarlyGame gs =
  case (nextPlayer gs, Map.keys (gameBoard gs)) of
    (Computer, [])     -> [Cell00, Cell02, Cell20, Cell22, Cell11]
    (Computer, [cell]) -> getBestCellsSecondMove cell
    _                  -> []

{-| Get the best cells to play as the second move of the game if the first
move of the game (your opponent's first move) was 'cell'.
-}
getBestCellsSecondMove :: Cell -> [Cell]
getBestCellsSecondMove cell =
  case cellToCellType cell of
    Center -> cellTypeToCells Corner
    _      -> cellTypeToCells Center

{-| Based on the given GameState, return the list of Cells that would be
best for the Computer if that cell were played into on the current turn.
-}
getBestCells :: GameState -> Either (MoveError Player) [Cell]
getBestCells gs = do
  cellScores <- scoreCellChoices gs
  let bestScore = snd $ maximumBy (compare `on` snd) cellScores
  pure $ map fst $ filter (eqRelErr 0.0001 bestScore . snd) cellScores

scoreCellChoices :: GameState -> Either (MoveError Player) [(Cell, Double)]
scoreCellChoices gs = do
  tree <- makeChoiceTree ([], gs, Nothing)
  let cellCounts = getCellCounts tree
      scoreCell (cell, counts) = (cell, scoreOutcomeCount counts)
  pure $ map scoreCell cellCounts

type OutcomeCount = (Int, Int, Int)
type CountMap = Map Cell OutcomeCount

mergeCountMaps :: [CountMap] -> CountMap
mergeCountMaps = Map.unionsWith addCounts

addCounts :: OutcomeCount -> OutcomeCount -> OutcomeCount
addCounts (w1, d1, l1) (w2, d2, l2) = (w1 + w2, d1 + d2, l1 + l2)

getPercents :: OutcomeCount -> (Double, Double, Double)
getPercents (w, d, l) = (w' / total, d' / total, l' / total)
  where w' = fromIntegral w
        d' = fromIntegral d
        l' = fromIntegral l
        total = fromIntegral $ w + d + l

scoreOutcomeCount :: OutcomeCount -> Double
scoreOutcomeCount count = w - l
  where (w, _, l) = getPercents count

getCellCounts :: ChoiceTree -> [(Cell, OutcomeCount)]
getCellCounts tree =
    Map.toList $ mergeCountMaps $ foldr go [] $ flatten tree
  where go (c:_, _, Just outcome) countMaps =
          Map.fromList [(c, scoreOutcome outcome)] : countMaps
        go _ countMaps = countMaps

scoreOutcome :: GameOutcome Player -> OutcomeCount
scoreOutcome (Winner Computer) = (1,0,0)
scoreOutcome Draw = (0,1,0)
scoreOutcome (Winner Human) = (0,0,1)

type ChoiceTuple = ([Cell], GameState, Maybe (GameOutcome Player))

type ChoiceTree = Tree ChoiceTuple

makeChoiceTree :: ChoiceTuple -> Either (MoveError Player) ChoiceTree
makeChoiceTree ct@(cells, gs, maybeOutcome)
  | isJust maybeOutcome = pure $ Node ct []
  | otherwise = do
      tuples <- traverse (makeChoiceTuple gs cells)
                  (Just <$> getObviousMoves gs)
      choiceForest <- traverse makeChoiceTree tuples
      pure $ Node ct choiceForest

makeChoiceTuple :: GameState
                -> [Cell]
                -> Maybe Cell
                -> Either (MoveError Player) ChoiceTuple
makeChoiceTuple gs cells Nothing = pure (cells, gs, checkForOutcome gs)
makeChoiceTuple gs cells (Just cell) = do
  gs' <- execStateT (performMove cell) gs
  predictedOutcome <- predictOutcome gs'
  pure (cells ++ [cell], gs', predictedOutcome)

{-| Try to predict the outcome of a game based on a GameState.  This function
isn't very sophisticated.  If there are no obvious moves to be made (winning
moves or blocking moves) this function will return Nothing.
-}
predictOutcome :: GameState
               -> Either (MoveError Player) (Maybe (GameOutcome Player))
predictOutcome gs =
    case (winMoveExists, numBlockMoves, checkForOutcome gs) of
      (_, _, Just outcome) -> pure $ Just outcome
      -- ^ If there is already an outcome, return it
      (False, 0, _)        -> pure Nothing
      -- ^ If there aren't any winning moves or blocking moves, return Nothing
      (False, 1, _)        -> do let blockMove = head $ getBlockingMoves gs
                                 gs' <- execStateT (performMove blockMove) gs
                                 predictOutcome gs'
      -- ^ If there is one blocking move, do that move and recurse.
      (False, _, _)        -> pure $ Just $ Winner $ opponent
      -- ^ If there are many moves that need blocked, current player has lost.
      (True, _, _)         -> pure $ Just $ Winner $ currentPlayer
      -- ^ If there are winning moves, current player has won.
  where winMoveExists = length (getWinningMoves gs) > 0
        numBlockMoves = length $ getBlockingMoves gs
        currentPlayer = nextPlayer gs
        opponent      = flipPlayer currentPlayer

{-| Return a list of cells that it would be logical for the current
player to play into.  If there are any moves that would allow the current
player to win the game, return those cells.  If there are no cells that would
allow the current player to win the game, but there are cells that the current
player needs to play into to prevent the opponent from winning, return those
blocking cells.  If there are no winning cells or blocking cells, just return
the list of empty cells.
-}
getObviousMoves :: GameState -> [Cell]
getObviousMoves gs =
  case (getWinningMoves gs, getBlockingMoves gs) of
    ([], [])          -> emptyCells $ gameBoard gs
    -- ^ if no winning moves and no blocking moves, return all available moves
    ([], blockingMoves)  -> blockingMoves
    -- ^ if no win moves but there are blocking moves, return blocking moves
    (winningMoves, _) -> winningMoves
    -- ^ if there are winning moves, return those

{-| Get the list of Cells that the nextPlayer could play into to win the game
on their current turn.
-}
getWinningMoves :: GameState -> [Cell]
getWinningMoves gs = filter (isWinningMove gs) (emptyCells $ gameBoard gs)

{-| Return True if the nextPlayer would win the game by playing into the given
Cell.
-}
isWinningMove :: GameState -> Cell -> Bool
isWinningMove gs = isThreatCell (gameBoard gs) (nextMark gs)

{-| Get the list of Cells that the current player must play into to prevent
the opponent from winning in that Cell on their next turn.
-}
getBlockingMoves :: GameState -> [Cell]
getBlockingMoves gs = filter (isBlockingMove gs) (emptyCells $ gameBoard gs)

{-| Return True if playing into the given Cell would block the opponent from
winning the game on their next turn.
-}
isBlockingMove :: GameState -> Cell -> Bool
isBlockingMove gs = isThreatCell (gameBoard gs) (flipMark (nextMark gs))

{-| Return True if the given Cell is part of a Line where one more Mark equal
to @threatMark@ would complete the line and win the game.
-}
isThreatCell :: Board -> Mark -> Cell -> Bool
isThreatCell board threatMark cell =
    cell `Map.notMember` board && (any isThreat $ getAllLineMarks board cell)
  where threatsOnly (m1, m2, m3) = filter (== Just threatMark) [m1, m2, m3]
        isThreat marks = length (threatsOnly marks) == 2
