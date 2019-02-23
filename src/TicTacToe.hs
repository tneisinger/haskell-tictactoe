{-# LANGUAGE RecordWildCards #-}

module TicTacToe where

import Control.Monad.State
import Control.Monad.Except
import Data.Tree (flatten, Tree(..))
import Data.Char (toUpper)
import Data.Function (on)
import Data.Maybe (isJust, fromMaybe)
import Data.List (sort, maximumBy)
import Data.Map (Map)
import Numeric.MathFunctions.Comparison (eqRelErr)
import System.Random
import qualified Data.Map as Map
import qualified Data.Set as Set

-- | A simple datatype to represent an X or an O in a tic-tac-toe game.
data Mark = X | O
  deriving (Eq, Show, Read)

-- | A convenience function for when you need the opposite Mark value.
flipMark :: Mark -> Mark
flipMark X = O
flipMark O = X

-- | A type to represent the 9 cells of a tic-tac-toe board.
data Cell = Cell00 | Cell01 | Cell02
          | Cell10 | Cell11 | Cell12
          | Cell20 | Cell21 | Cell22
          deriving (Eq, Show, Enum, Ord, Read)

{-| A Board is a Map from a Cell to a Mark.  If a Cell is not in the Map,
that corresponds to that cell being empty.
-}
type Board = (Map Cell Mark)

{-| A blank tic-tac-toe board, implemented as an empty Map from Cell to
Marks.
-}
blankBoard :: Board
blankBoard = Map.empty

{-| A sum type with 8 constructors, one to represent each of the valid lines
on a tic-tac-toe board.
-}
data Line = Line00to02
          | Line10to12
          | Line20to22
          | Line00to20
          | Line01to21
          | Line02to22
          | Line00to22
          | Line20to02
          deriving (Eq, Show, Enum)

-- | Given a Line, return the Cells that correspond to that line.
line2Cells :: Line -> (Cell, Cell, Cell)
line2Cells Line00to02 = (Cell00, Cell01, Cell02)
line2Cells Line10to12 = (Cell10, Cell11, Cell12)
line2Cells Line20to22 = (Cell20, Cell21, Cell22)
line2Cells Line00to20 = (Cell00, Cell10, Cell20)
line2Cells Line01to21 = (Cell01, Cell11, Cell21)
line2Cells Line02to22 = (Cell02, Cell12, Cell22)
line2Cells Line00to22 = (Cell00, Cell11, Cell22)
line2Cells Line20to02 = (Cell20, Cell11, Cell02)

{-| A data type that is returned when a move cannot be completed.
Although the 'a' is fully polymorphic, this type will exclusively be used as
either 'MoveError Mark or 'MoveError Player'.
-}
data MoveError a = CellFull Cell Mark
                        | GameOver (GameOutcome a)
                        | NotTurnOf a
                        | UnknownMoveErr Board String

-- | Used in the Show instance for the MoveError type.
showMoveErrorPrefix :: String
showMoveErrorPrefix = "MoveError: "

-- | Give a description of a MoveError when using show.
instance Show a => Show (MoveError a) where
  show (GameOver outcome) =
    concat [ showMoveErrorPrefix
           , "No other moves can be made because the game is over.\n"
           , "Game outcome: " ++ show outcome
           ]
  show (NotTurnOf p) =
    concat [ showMoveErrorPrefix
           , show p ++ " tried to put a mark on the board when it was not "
           , show p ++ "'s turn."
           ]
  show (UnknownMoveErr board msg) =
    concat [ showMoveErrorPrefix
           , "An unknown MoveError occurred.\n"
           , "Error Message: " ++ msg ++ "\n"
           , "At the time of the error, the board was:\n\n"
           , showBoard board ++ "\n\n"
           ]

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

-- | Get the player that is playing as the given mark.
getPlayer :: GameState -> Mark -> Player
getPlayer gs mark = if computerMark gs == mark then Computer else Human

{-| Either return a new Board with the given Mark added to the given Cell, or
return an MoveError.  An MoveError will be returned if the cell
to be filled already contains a Mark, or if it isn't the turn of the given
Mark (based on a comparison of the number of Xs and Os already on the board).
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

{-| As the names suggest, this monad is similar to the TicTacToe monad, but
with IO. This monad is used for the playable text version of the game.

StateT is used to handle the tic-tac-toe GameState. The GameState also holds
the StdGen that is used by the Computer player to select a random best move
from the available moves. ExceptT is used to return a MoveError if one occurs.
-}
type TicTacToeIO a = StateT GameState (ExceptT (MoveError Player) IO) a

{-| Run a TicTacToeIO operation against a GameState and get back the result of
that operation.
-}
evalTicTacToeIO :: TicTacToeIO a
               -> GameState
               -> IO (Either (MoveError Player) a)
evalTicTacToeIO tttIO = runExceptT . evalStateT tttIO

{-| Run a TicTacToeIO operation against a GameState and get the resulting
GameState as the return value.
-}
execTicTacToeIO :: TicTacToeIO a
              -> GameState
              -> IO (Either (MoveError Player) GameState)
execTicTacToeIO tttIO = runExceptT . execStateT tttIO

{-| As the names suggest, this monad is similar to the TicTacToeIO monad, but
without IO.

StateT is used to handle the tic-tac-toe GameState.  The GameState also holds
the StdGen that is used by the Computer player to select a random best move
from the available moves. Either is used to return a MoveError if one occurs.
-}
type TicTacToe a = StateT GameState (Either (MoveError Player)) a

-- | Turn a TicTacToe operation into a TicTacToeIO operation.
liftTicTacToe :: TicTacToe a -> TicTacToeIO a
liftTicTacToe f = do
  gs <- get
  case runStateT f gs of
    Left err -> lift $ throwError err
    Right (a, gs') -> put gs' >> pure a

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
    (Computer, [])       -> [Cell00, Cell02, Cell20, Cell22]
    (Computer, [Cell00]) -> [Cell22]
    (Computer, [Cell02]) -> [Cell20]
    (Computer, [Cell20]) -> [Cell02]
    (Computer, [Cell22]) -> [Cell00]
    (Computer, [Cell11]) -> [Cell00, Cell02, Cell20, Cell22]
    (Computer, [Cell01]) -> [Cell11]
    (Computer, [Cell10]) -> [Cell11]
    (Computer, [Cell12]) -> [Cell11]
    (Computer, [Cell21]) -> [Cell11]
    _                    -> []

-- | A convenience function for making arbitrary GameStates
makeSampleGS :: Mark -> Int -> [Cell] -> Either (MoveError Player) GameState
makeSampleGS initMark genInt cells = do
  let gs = initGameState initMark genInt
      moves = sequence_ $ map performMove cells
  execStateT moves gs

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
      scoreCell = \(cell, counts) -> (cell, scoreOutcomeCount counts)
  pure $ map scoreCell cellCounts

{-| This function is used with the foldTree function to create a CountMap
out of a ChoiceTree.
-}
makeCountMap :: ChoiceTuple -> [CountMap] -> CountMap
makeCountMap (c:_, _, Just outcome) countMaps =
    Map.insert c countForC countsMap
  where countForC = fromMaybe countForThis mCountsForC
        mCountsForC = addCounts countForThis <$> Map.lookup c countsMap
        countForThis = case outcome of
                          Winner Computer -> (1, 0, 0)
                          Draw            -> (0, 1, 0)
                          Winner Human    -> (0, 0, 1)
        countsMap = mergeCountMaps countMaps
makeCountMap (_, _, Nothing) countMaps = mergeCountMaps countMaps

type OutcomeCount = (Int, Int, Int)
type CountMap = Map Cell OutcomeCount

mergeCountMaps :: [CountMap] -> CountMap
mergeCountMaps countMaps = Map.unionsWith addCounts countMaps

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
  where go (_, _, Nothing) countMaps = countMaps
        go (c:_, _, Just outcome) countMaps =
          Map.fromList [(c, scoreOutcome outcome)] : countMaps

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
      tuples <- traverse (makeChoiceTuple gs cells) (Just <$> getSmartMoves gs)
      choiceForest <- traverse makeChoiceTree tuples
      pure $ Node ct choiceForest

makeChoiceTuple :: GameState
                -> [Cell]
                -> Maybe Cell
                -> Either (MoveError Player) ChoiceTuple
makeChoiceTuple gs cells Nothing = pure $ (cells, gs, checkGSForOutcome gs)
makeChoiceTuple gs cells (Just cell) = do
  gs' <- execStateT (performMove cell) gs
  predictedOutcome <- getPredictedOutcome gs'
  pure (cells ++ [cell], gs', predictedOutcome)

getPredictedOutcome :: GameState
                    -> Either (MoveError Player) (Maybe (GameOutcome Player))
getPredictedOutcome gs =
  case (getWinMoves gs, getBlockMoves gs, checkGSForOutcome gs) of
    (_, _, Just outcome) -> pure $ Just outcome
    ([], [], _) -> pure Nothing
    ([], (_:_:_), _) -> pure (Just $ Winner $ flipPlayer $ nextPlayer gs)
    ([], [c], _) -> do
      gs' <- execStateT (performMove c) gs
      getPredictedOutcome gs'
    _ -> pure (Just $ Winner $ nextPlayer gs)

getSmartMoves :: GameState -> [Cell]
getSmartMoves gs =
  case (getWinMoves gs, getBlockMoves gs) of
    ([], [])          -> emptyCells $ gameBoard gs
    -- ^ if no win moves and no blocking moves, just return the available moves
    ([], [c])         -> [c]
    -- ^ if no win moves but one blocking move, the block is the smart move
    ([], (_:_:_))     -> []
    -- ^ if multiple cells need to be blocked, there are no smart moves to make
    (winningMoves, _) -> winningMoves
    -- ^ if there are winning moves, return those

{-| Get the list of Cells that the nextPlayer could play into to win the game
on their current turn.
-}
getWinMoves :: GameState -> [Cell]
getWinMoves gs = filter (isWinningMove gs) (emptyCells $ gameBoard gs)

{-| Return True if the nextPlayer would win the game by playing into the given
Cell.
-}
isWinningMove :: GameState -> Cell -> Bool
isWinningMove gs cell = hasThreatLine (gameBoard gs) (nextMark gs) cell

{-| Get the list of Cells that the current player must play into to prevent
the opponent from winning in that Cell on their next turn.
-}
getBlockMoves :: GameState -> [Cell]
getBlockMoves gs = filter (isBlockingMove gs) (emptyCells $ gameBoard gs)

{-| Return True if playing into the given Cell would block the opponent from
winning the game on their next turn.
-}
isBlockingMove :: GameState -> Cell -> Bool
isBlockingMove gs cell =
  hasThreatLine (gameBoard gs) (flipMark (nextMark gs)) cell

{-| Return True if the given Board gives the player playing as @threatMark@
more than one opportunity to win in just one move.
-}
hasMultipleThreats :: Board -> Mark -> Bool
hasMultipleThreats board threatMark = length threats > 1
  where threats = filter (hasThreatLine board threatMark) (emptyCells board)

{-| Return True if the given Cell is part of a Line where one more Mark equal
to @threatMark@ would complete the line and win the game.
-}
hasThreatLine :: Board -> Mark -> Cell -> Bool
hasThreatLine board threatMark cell = any isThreat $ getAllLineMarks board cell
    where threatsOnly (m1, m2, m3) = filter (== Just threatMark) [m1, m2, m3]
          isThreat marks = length (threatsOnly marks) == 2

-- | Get the list of Cells corresponding to the cells that are currently empty
emptyCells :: Board -> [Cell]
emptyCells board =
  let setFilledCells = Set.fromList $ Map.keys board
      setAllCells = Set.fromList [Cell00 ..]
   in sort $ Set.toList $ Set.difference setAllCells setFilledCells

-- | Predicate to determine if the given Line goes through the given Cell.
lineCrosses :: Cell -> Line -> Bool
lineCrosses cell line =
  let (c1, c2, c3) = line2Cells line
   in c1 == cell || c2 == cell || c3 == cell

-- | Get the list of Lines that go through the given Cell.
getLinesThatCross :: Cell -> [Line]
getLinesThatCross cell = filter (lineCrosses cell) [Line00to02 ..]

-- | Get the contents of the Cells that the given Line goes through.
getLineMarks :: Board -> Line -> (Maybe Mark, Maybe Mark, Maybe Mark)
getLineMarks board line =
  let (c1, c2, c3) = line2Cells line
      m1 = Map.lookup c1 board
      m2 = Map.lookup c2 board
      m3 = Map.lookup c3 board
   in (m1,m2,m3)

{-| For each line that crosses the given Cell, return a list of what is on the
Board under those lines.
|-}
getAllLineMarks :: Board -> Cell -> [(Maybe Mark, Maybe Mark, Maybe Mark)]
getAllLineMarks board = map (getLineMarks board) . getLinesThatCross

{-| Given a board that has an outcome, return an informative MoveError.
This function assumes that the given board has an outcome (win or draw).  An
UnknownMoveErr will be returned if this function is called on a board that
doesn't have an outcome. This function should only be used in situations where
you know that the board does in fact have an outcome.
-}
getOutcomeMoveError :: Board -> String -> MoveError Mark
getOutcomeMoveError board errMsg =
  case checkForOutcome board of
    Just outcome -> GameOver outcome
    Nothing      -> UnknownMoveErr board errMsg

-- | Return a String representation of a Board
showBoard :: Board -> String
showBoard board =
  let aboveRow = "    |   |    "
      belowRow = " ___|___|___ "
      bottom   = "    |   |"
      row1      = showLine board Line00to02
      row2      = showLine board Line10to12
      row3      = showLine board Line20to22
   in unlines [ aboveRow, row1, belowRow,
                aboveRow, row2, belowRow,
                aboveRow, row3, bottom ]
{-| Return a String representation of the contents of a Cell.
Display `Nothing` as a space (" "), `Just X` as "X", and `Just O` as "O"
-}
showCellContents :: Maybe Mark -> String
showCellContents Nothing = " "
showCellContents (Just m) = show m

-- | Convenience function for displaying the contents of a cell of the board
showCell :: Board -> Cell -> String
showCell board cell = showCellContents $ Map.lookup cell board

-- | Return the String representation of a horizontal line of the Board
showLine :: Board -> Line -> String
showLine board line =
  let (c1, c2, c3) = getLineMarks board line
   in concat [ "  "
             , showCellContents c1
             , " | "
             , showCellContents c2
             , " | "
             , showCellContents c3
             ]

-- | A simple Type for the players of the game.
data Player = Computer | Human
  deriving (Eq, Show)

-- | A convenience function for getting the opposite Player
flipPlayer :: Player -> Player
flipPlayer Computer = Human
flipPlayer Human = Computer

-- | The State of a tic-tac-toe game.
data GameState = GameState { stdGen :: StdGen
                           , nextPlayer :: Player
                           , computerMark :: Mark
                           , gameBoard :: Board
                           } deriving (Show)

-- | Get the Mark for the Human Player from a GameState
humanMark :: GameState -> Mark
humanMark = flipMark . computerMark

-- | Get the Mark that will be played next in the game
nextMark :: GameState -> Mark
nextMark gs = if nextPlayer gs == Computer
                 then computerMark gs
                 else humanMark gs

{-| A Type representing the outcome of a game. This will be used as either
'GameOutcome Mark' or 'GameOutcome Player'.
-}
data GameOutcome a = Winner a | Draw
  deriving (Eq, Show)

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

{-| Check if the given GameState represents a finished game.  If the game isn't
finished yet, return `Nothing`.  If the game is finished and it is a draw,
return `Just Draw`.  If the Human won the game, return `Just (Winner Human)`.
If the Computer won the game, return `Just (Winner Computer)`
-}
checkGSForOutcome :: GameState -> Maybe (GameOutcome Player)
checkGSForOutcome gs =
  case checkForOutcome (gameBoard gs) of
    Nothing -> Nothing
    Just Draw -> Just Draw
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

{-| Ask the human which Mark they would like to play as.  The human's response
will determine if the human or the computer will move first, since X always
moves first.
-}
askWhichMark :: IO Mark
askWhichMark = do
  putStrLn "Would you like to play as X or O?\n"
  putStr " > "
  response <- (map toUpper) <$> getLine
  case reads response of
    [] -> putStrLn "Please enter an X or an O..." >> askWhichMark
    [(mark, _)] -> return mark

{-| Ask the player which Cell they would like to play into.  Return the Cell
chosen by the player.
-}
askWhichCell :: Board -> IO Cell
askWhichCell board = do
  let choices = emptyCells board
  putStrLn "Enter a free cell from the list below: "
  putStrLn $ show choices
  cellStr <- getLine
  case reads cellStr of
    [] -> putStrLn "Invalid input. Try again." >> askWhichCell board
    ((cell,_):_) ->
      if cell `notElem` choices
         then putStrLn "That cell is full. Try again" >> askWhichCell board
         else return cell

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

-- | Print the GameState to the screen in a convenient way.
printGameState :: GameState -> IO ()
printGameState gs =
  let lastLine = case checkGSForOutcome gs of
                   Nothing -> show (nextPlayer gs) ++ " moves next..."
                   Just (Winner winner) -> show winner ++ " wins!"
                   Just Draw -> "Draw! Nobody wins :("
   in putStrLn $ unlines [ "Game Board:\n"
                         , showBoard (gameBoard gs)
                         , "Human: " ++ show (humanMark gs)
                         , "Computer: " ++ show (computerMark gs)
                         , lastLine
                         ]

-- | The main loop for the text version of this game.
textGameLoop :: TicTacToeIO ()
textGameLoop = do
  gs <- get
  liftIO $ printGameState gs
  case (checkGSForOutcome gs, nextPlayer gs) of
    (Just _, _)         -> pure ()
    (Nothing, Computer) -> liftTicTacToe doComputerMove >> textGameLoop
    (Nothing, Human)    -> do
      cell <- liftIO $ askWhichCell (gameBoard gs)
      liftTicTacToe $ doHumanMove cell
      textGameLoop

-- | Entrypoint for running the text version of this game.
playTextGame :: IO ()
playTextGame = do
  humanMark <- askWhichMark
  moveErrorOrUnit <- evalTicTacToeIO textGameLoop (initGameState humanMark 42)
  case moveErrorOrUnit of
    Left err -> print err
    _        -> pure ()

