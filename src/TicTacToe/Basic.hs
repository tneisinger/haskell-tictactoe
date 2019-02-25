module TicTacToe.Basic
       ( checkGSForOutcome
       , doHumanMove
       , emptyCells
       , fillCell
       , flipMark
       , flipPlayer
       , getAllLineMarks
       , getWinningLines
       , humanMark
       , initGameState
       , makeSampleGS
       , maybeIntToCell
       , newGameState
       , nextMark
       , performMove
       , promoteMoveError
       , showBoard
       , showBoardWithCellNums
       , showCellContents
       ) where

import Control.Monad.State (execStateT, put, get, lift)
import Data.Maybe (isJust)
import System.Random (mkStdGen)

import TicTacToe.Types (GameState(..), Cell(..), Player(..), MoveError(..),
                        Mark(..), TicTacToe, Line(..), Board, GameOutcome(..))

import qualified Data.Map as Map


-- | A convenience function for when you need the opposite Mark value.
flipMark :: Mark -> Mark
flipMark X = O
flipMark O = X

{-| A blank tic-tac-toe board, implemented as an empty (Map Cell Mark).
-}
blankBoard :: Board
blankBoard = Map.empty

-- | Given a Line, return the Cells that correspond to that line.
lineToCells :: Line -> (Cell, Cell, Cell)
lineToCells Line00to02 = (Cell00, Cell01, Cell02)
lineToCells Line10to12 = (Cell10, Cell11, Cell12)
lineToCells Line20to22 = (Cell20, Cell21, Cell22)
lineToCells Line00to20 = (Cell00, Cell10, Cell20)
lineToCells Line01to21 = (Cell01, Cell11, Cell21)
lineToCells Line02to22 = (Cell02, Cell12, Cell22)
lineToCells Line00to22 = (Cell00, Cell11, Cell22)
lineToCells Line20to02 = (Cell20, Cell11, Cell02)

-- | Using a GameState, convert a 'MoveError Mark' into a 'MoveError Player'
promoteMoveError :: GameState
                 -> MoveError Mark
                 -> MoveError Player
promoteMoveError _ (CellFull c m) = CellFull c m
promoteMoveError _ (GameOver Draw) = GameOver Draw
promoteMoveError _ (UnknownMoveErr b msg) = UnknownMoveErr b msg
promoteMoveError gs (NotTurnOf mark) = NotTurnOf $ getPlayer gs mark
promoteMoveError gs (GameOver (Winner mark)) =
  GameOver $ Winner $ getPlayer gs mark

{-| Either return a new Board with the given Mark added to the given Cell, or
return a MoveError.  A MoveError will be returned if the cell to be filled
already contains a Mark, or if it isn't the turn of the given Mark (based on a
comparison of the number of Xs and Os already on the board).
-}
fillCell :: Cell -> Mark -> Board -> Either (MoveError Mark) Board
fillCell cell mark board =
  case (isNextTurn board mark, Map.lookup cell board) of
    (False, _)   -> Left $ NotTurnOf mark
    (_, Just m)  -> Left $ CellFull cell m
    (_, Nothing) -> Right $ Map.insert cell mark board

-- | Return the number of Xs and Os on a board
countMarks :: Board -> (Int, Int)
countMarks board = foldr go (0,0) $ Map.elems board
  where go mark (numXs, numOs) = if mark == X
                              then (numXs + 1, numOs)
                              else (numXs, numOs + 1)

-- | Determine if the given Mark should get the next move on the board
isNextTurn :: Board -> Mark -> Bool
isNextTurn board mark = if numMoreXs > 0 then mark == O else mark == X
  where (numXs, numOs) = countMarks board
        numMoreXs = numXs - numOs

-- | Get the list of Cells that are currently empty
emptyCells :: Board -> [Cell]
emptyCells board = filter (flip Map.notMember board) [Cell00 ..]

-- | A predicate to determine if the given Line goes through the given Cell.
lineCrosses :: Cell -> Line -> Bool
lineCrosses cell line =
  let (c1, c2, c3) = lineToCells line
   in c1 == cell || c2 == cell || c3 == cell

-- | Get the list of Lines that go through the given Cell.
getLinesThatCross :: Cell -> [Line]
getLinesThatCross cell = filter (lineCrosses cell) [Line00to02 ..]

-- | Get the contents of the Cells that the given Line goes through.
getLineMarks :: Board -> Line -> (Maybe Mark, Maybe Mark, Maybe Mark)
getLineMarks board line =
  let (c1, c2, c3) = lineToCells line
      m1 = Map.lookup c1 board
      m2 = Map.lookup c2 board
      m3 = Map.lookup c3 board
   in (m1,m2,m3)

{-| For each line that crosses the given Cell, return a list of what is on the
Board under those lines.
|-}
getAllLineMarks :: Board -> Cell -> [(Maybe Mark, Maybe Mark, Maybe Mark)]
getAllLineMarks board = map (getLineMarks board) . getLinesThatCross

showBoardWith :: (Board -> Line -> String) -> Board -> String
showBoardWith f board =
  let aboveRow  = "    |   |    "
      belowRow  = " ___|___|___ "
      bottom    = "    |   |    "
      row1      = f board Line00to02
      row2      = f board Line10to12
      row3      = f board Line20to22
   in unlines [ aboveRow
              , row1
              , belowRow
              , aboveRow
              , row2
              , belowRow
              , aboveRow
              , row3
              , bottom ]

-- | Return a String representation of a Board
showBoard :: Board -> String
showBoard = showBoardWith showLine

{-| Return a String representation of a Board, but with all the empty cells
filled with an integer.  The cells will be filled with the integers 1-9.  When
empty, Cell00 will always display "1", Cell01 will always display "2", and so
on.
-}
showBoardWithCellNums :: Board -> String
showBoardWithCellNums = showBoardWith showLineWithCellNums

{-| Return a String representation of the contents of a Cell.
Display `Nothing` as a space (" "), `Just X` as "X", and `Just O` as "O"
-}
showCellContents :: Board -> Cell -> String
showCellContents = showCellContentsWith (const " ")

showCellContentsOrNum :: Board -> Cell -> String
showCellContentsOrNum = showCellContentsWith (show . cellToInt)

showCellContentsWith :: (Cell -> String) -> Board -> Cell -> String
showCellContentsWith f board cell =
  case Map.lookup cell board of
    Nothing   -> f cell
    Just mark -> show mark

cellToInt :: Cell -> Int
cellToInt Cell00 = 1
cellToInt Cell01 = 2
cellToInt Cell02 = 3
cellToInt Cell10 = 4
cellToInt Cell11 = 5
cellToInt Cell12 = 6
cellToInt Cell20 = 7
cellToInt Cell21 = 8
cellToInt Cell22 = 9

maybeIntToCell :: Int -> Maybe Cell
maybeIntToCell 1 = Just Cell00
maybeIntToCell 2 = Just Cell01
maybeIntToCell 3 = Just Cell02
maybeIntToCell 4 = Just Cell10
maybeIntToCell 5 = Just Cell11
maybeIntToCell 6 = Just Cell12
maybeIntToCell 7 = Just Cell20
maybeIntToCell 8 = Just Cell21
maybeIntToCell 9 = Just Cell22
maybeIntToCell _ = Nothing

showLineWith :: (Board -> Cell -> String) -> Board -> Line -> String
showLineWith f board line =
  let (c1, c2, c3) = lineToCells line
   in concat [ "  "
             , f board c1
             , " | "
             , f board c2
             , " | "
             , f board c3
             , "  "
             ]

-- | Return the String representation of a horizontal line of the Board
showLine :: Board -> Line -> String
showLine = showLineWith showCellContents

-- | Return the String representation of a horizontal line of the Board
showLineWithCellNums :: Board -> Line -> String
showLineWithCellNums = showLineWith showCellContentsOrNum

-- | A convenience function for getting the opposite Player
flipPlayer :: Player -> Player
flipPlayer Computer = Human
flipPlayer Human = Computer

-- | Get the Mark for the Human Player from a GameState
humanMark :: GameState -> Mark
humanMark = flipMark . computerMark

-- | Get the Mark that will be played next in the game
nextMark :: GameState -> Mark
nextMark gs = if nextPlayer gs == Computer
                 then computerMark gs
                 else humanMark gs

-- | Get the player that is playing as the given mark.
getPlayer :: GameState -> Mark -> Player
getPlayer gs mark = if computerMark gs == mark then Computer else Human

{-| Given a Board, maybe return a GameOutcome Mark. If Nothing is returned,
that means that the game hasn't finished yet.
-}
checkForOutcome :: Board -> Maybe (GameOutcome Mark)
checkForOutcome board =
  case filter isJust $ map (checkLineForWinner board) [Line00to02 ..] of
    (Just winnerMark:_) -> Just (Winner winnerMark)
    [] -> if length (Map.toList board) == 9
             then Just Draw
             else Nothing
    _  -> Nothing

{-| Check if the given GameState represents a finished game.  If the game isn't
finished yet, return `Nothing`.  If the game is finished and it is a draw,
return `Just Draw`.  If the Human won the game, return `Just (Winner Human)`.
If the Computer won the game, return `Just (Winner Computer)`
-}
checkGSForOutcome :: GameState -> Maybe (GameOutcome Player)
checkGSForOutcome gs =
  case checkForOutcome (gameBoard gs) of
    Nothing            -> Nothing
    Just Draw          -> Just Draw
    Just (Winner mark) -> if mark == (computerMark gs)
                             then Just (Winner Computer)
                             else Just (Winner Human)

-- | Check if there is a winner on the given Line. Maybe return the winner.
checkLineForWinner :: Board -> Line -> Maybe Mark
checkLineForWinner board line =
  case getLineMarks board line of
    (Just X, Just X, Just X) -> Just X
    (Just O, Just O, Just O) -> Just O
    _                        -> Nothing

-- | Get a list of the lines that contain three Xs or three Os.
getWinningLines :: Board -> [Line]
getWinningLines board =
  filter (isJust . checkLineForWinner board) [Line00to02 ..]

-- | Create an initial GameState based on the Mark the human wants to play as.
initGameState :: Mark ->  Int -> GameState
initGameState X i = GameState { stdGen = mkStdGen i
                              , nextPlayer = Human
                              , computerMark = O
                              , gameBoard = blankBoard
                              }
initGameState O i = GameState { stdGen = mkStdGen i
                              , nextPlayer = Computer
                              , computerMark = X
                              , gameBoard = blankBoard
                              }

{-| Reset the GameState to a new game, but keeping the StdGen so that
the randomness will carry forward into the new game.
-}
newGameState :: GameState -> Mark -> GameState
newGameState gs mark = (initGameState mark 0) { stdGen = stdGen gs }

{-| Return a StateT that will either play the correct Mark value into the given
Cell, or will return an 'MoveError Player'.
-}
doHumanMove :: Cell -> TicTacToe ()
doHumanMove cell = do
  gs <- get
  case nextPlayer gs of
    Computer -> lift $ Left (NotTurnOf Human)
    Human -> performMove cell

{-| Return a StateT that will either play the correct Mark (X or O) into the
given Cell, or will return an error message.
-}
performMove :: Cell -> TicTacToe ()
performMove cell = do
  gs <- get
  case checkGSForOutcome gs of
    Just outcome -> lift $ Left (GameOver outcome)
    Nothing -> do
      let np = nextPlayer gs
          mark = if np == Computer then (computerMark gs) else (humanMark gs)
      case fillCell cell mark (gameBoard gs) of
        Left err -> lift $ Left $ promoteMoveError gs err
        Right newBoard -> put gs { nextPlayer = flipPlayer np
                                 , gameBoard = newBoard }

-- | A convenience function for making arbitrary GameStates
makeSampleGS :: Mark -> Int -> [Cell] -> Either (MoveError Player) GameState
makeSampleGS initMark genInt cells = do
  let gs = initGameState initMark genInt
      moves = sequence_ $ map performMove cells
  execStateT moves gs
