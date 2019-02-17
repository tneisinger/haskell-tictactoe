{-# LANGUAGE RecordWildCards #-}

module TicTacToe where

import Control.Monad.State
import Data.Char (toUpper)
import Data.Function (on)
import Safe (headMay)
import Data.Maybe (maybeToList, isJust)
import Data.List (sortBy, sort)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Set as Set

-- |A simple datatype to represent an X or an O in a tic-tac-toe game.
data Mark = X | O
  deriving (Eq, Show, Read)

-- |A convenience function for when you need the opposite Mark value.
flipMark :: Mark -> Mark
flipMark X = O
flipMark O = X

-- |A type to represent the 9 cells of a tic-tac-toe board.
data Cell = Cell00 | Cell01 | Cell02
          | Cell10 | Cell11 | Cell12
          | Cell20 | Cell21 | Cell22
          deriving (Eq, Show, Enum, Ord, Read)

{-|
  A Board is a Map from a Cell to a Mark.  If a Cell is not in the Map,
  that corresponds to that cell being empty.
-}
type Board = (Map Cell Mark)

{-|
  A blank tic-tac-toe board, implemented as an empty Map from Cell to
  Marks
-}
blankBoard :: Board
blankBoard = Map.empty

{-|
  A sum type with 8 constructors, one to represent each of the valid lines
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

-- |Given a Line, return the Cells that correspond to that line.
line2Cells :: Line -> (Cell, Cell, Cell)
line2Cells Line00to02 = (Cell00, Cell01, Cell02)
line2Cells Line10to12 = (Cell10, Cell11, Cell12)
line2Cells Line20to22 = (Cell20, Cell21, Cell22)
line2Cells Line00to20 = (Cell00, Cell10, Cell20)
line2Cells Line01to21 = (Cell01, Cell11, Cell21)
line2Cells Line02to22 = (Cell02, Cell12, Cell22)
line2Cells Line00to22 = (Cell00, Cell11, Cell22)
line2Cells Line20to02 = (Cell20, Cell11, Cell02)

{-|
  Either return a new Board with the given Mark added to the given Cell, or
  return an error string if the Board already has a Mark in that Cell.
-}
fillCell :: Cell -> Mark -> Board -> Either String Board
fillCell cell mark board =
  case Map.lookup cell board of
    Nothing -> Right $ Map.insert cell mark board
    Just m  -> Left $ show cell ++ " already contains " ++ show m

-- |Get the list of Cells corresponding to the cells that are currently empty
emptyCells :: Board -> [Cell]
emptyCells board =
  let setFilledCells = Set.fromList $ Map.keys board
      setAllCells = Set.fromList [Cell00 ..]
   in sort $ Set.toList $ Set.difference setAllCells setFilledCells

-- |Predicate to determine if the given Line goes through the given Cell.
lineCrosses :: Cell -> Line -> Bool
lineCrosses cell line =
  let (c1, c2, c3) = line2Cells line
   in c1 == cell || c2 == cell || c3 == cell

-- |Get the list of Lines that go through the given Cell.
getLinesThatCross :: Cell -> [Line]
getLinesThatCross cell = filter (lineCrosses cell) [Line00to02 ..]

-- |Get the contents of the Cells that the given Line goes through.
getLineMarks :: Board -> Line -> (Maybe Mark, Maybe Mark, Maybe Mark)
getLineMarks board line =
  let (c1, c2, c3) = line2Cells line
      m1 = Map.lookup c1 board
      m2 = Map.lookup c2 board
      m3 = Map.lookup c3 board
   in (m1,m2,m3)

{-|
  For each line that crosses the given Cell, return a list of what is on the
  Board under those lines.
|-}
getAllLineMarks :: Board -> Cell -> [(Maybe Mark, Maybe Mark, Maybe Mark)]
getAllLineMarks board = map (getLineMarks board) . getLinesThatCross

{-|
  This function is the heart of how the computer selects its next move.  It
  returns an integer corresponding to the relative importance of playing the
  next move into the line in question. A higher number means that it is more
  important to play on that line.  The first Mark input value determines which
  Mark (X or O) the AI should think of as "his."
-}
scoreLineMarks :: Mark -> (Maybe Mark, Maybe Mark, Maybe Mark) -> Int
scoreLineMarks myMark (c1, c2, c3) =
  let marks = concatMap maybeToList [c1, c2, c3]
   in case marks of
        [] -> 1
        [m] -> if m == myMark then 3 else 2
        [m1, m2] -> case (m1 == m2, m1 == myMark) of
                      (False, _)    -> 0
                      (True, False) -> 8
                      (True, True)  -> 24
        _ -> -1

{-|
  Return an Int that represents the relative importance of playing into the
  given Cell.  If the Cell would be a good place to play, return a higher Int.
  If it would be a poor place to play, return a lower Int.  The first Mark
  argument given to this function determines which Mark (X or O) that the AI
  will think of as "his."
-}
scoreCell :: Mark -> Board -> Cell -> Int
scoreCell myMark board cell =
  sum $ map (scoreLineMarks myMark) $ getAllLineMarks board cell

{-|
  For every Cell that is currently empty on the Board, assign an Int that
  represents the relative importance of playing into that cell next. The first
  Mark argument given to this function determines which Mark (X or O) that the
  AI will think of as "his."
-}
scoreEmptyCells :: Mark -> Board -> [(Cell, Int)]
scoreEmptyCells myMark board =
  map (\cID -> (cID, scoreCell myMark board cID)) $ emptyCells board

{-|
  Either return the Cell to play into next, or an error message. The first
  Mark argument given to this function determines which Mark (X or O) that the
  AI will think of as "his."
-}
selectNextMove :: Mark -> Board -> Either String Cell
selectNextMove myMark board =
  let emptyCellScores = scoreEmptyCells myMark board
   in case headMay $ reverse $ sortBy (compare `on` snd) emptyCellScores of
        Nothing       -> Left "There are no moves left to make."
        Just (cID, _) -> Right cID

-- |Return a String representation of a Board
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
{-|
  Return a String representation of the contents of a Cell.
  Display `Nothing` as a space (" "), `Just X` as "X", and `Just O` as "O"
-}
showCellContents :: Maybe Mark -> String
showCellContents Nothing = " "
showCellContents (Just m) = show m

-- |Convenience function for displaying the contents of a cell of the board
showCell :: Board -> Cell -> String
showCell board cell = showCellContents $ Map.lookup cell board

-- |Return the String representation of a horizontal line of the Board
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

-- |A simple Type for the players of the game.
data Player = Computer | Human
  deriving (Eq, Show)

-- |A convenience function for getting the opposite Player
flipPlayer :: Player -> Player
flipPlayer Computer = Human
flipPlayer Human = Computer

-- |The State of a tic-tac-toe game.
data GameState = GameState { nextPlayer :: Player
                           , computerMark :: Mark
                           , gameBoard :: Board
                           } deriving (Eq, Show)

-- |Get the Mark for the Human Player from a GameState
humanMark :: GameState -> Mark
humanMark = flipMark . computerMark

{-|
  A Type representing the outcome of a game. This will be used as either
  `GameOutcome Mark` or `GameOutcome Player`.
-}
data GameOutcome a = Winner a | Draw
  deriving (Eq, Show)

{-|
  Given a Board, maybe return a GameOutcome Mark.  If Nothing is returned, that
  means that the game hasn't finished yet.
-}
checkForOutcome :: Board -> Maybe (GameOutcome Mark)
checkForOutcome board =
  case filter isJust $ map (checkLineForWinner board) [Line00to02 ..] of
    (Just winnerMark:_) -> Just (Winner winnerMark)
    [] -> if length (Map.toList board) == 9
             then Just Draw
             else Nothing

{-|
  Check if the given GameState represents a finished game.  If the game isn't
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

-- |Check if there is a winner on the given Line. Maybe return the winner.
checkLineForWinner :: Board -> Line -> Maybe Mark
checkLineForWinner board line =
  case getLineMarks board line of
    (Just X, Just X, Just X) -> Just X
    (Just O, Just O, Just O) -> Just O
    _                        -> Nothing

{-|
  Ask the human which Mark they would like to play as.  The human's response
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

{-|
  Ask the player which Cell they would like to play into.  Return the Cell
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

-- |Create an initial GameState based on which Mark the human wants to play as.
initGameState :: Mark -> GameState
initGameState X = GameState { nextPlayer = Human
                            , computerMark = O
                            , gameBoard = blankBoard
                            }
initGameState O = GameState { nextPlayer = Computer
                            , computerMark = X
                            , gameBoard = blankBoard
                            }

{-|
  Return a StateT that will either play the correct Mark value into the given
  Cell, or will return a error message.
-}
doHumanMove :: Cell -> StateT GameState (Either String) ()
doHumanMove cell = do
  gameState <- get
  case nextPlayer gameState of
    Computer -> lift $ Left "It is not the human's turn."
    Human -> performMove cell

{-|
  Return a StateT that will either play the correct Mark (X or O) into the Cell
  chosen by the computer, or return an error message.
-}
doComputerMove :: StateT GameState (Either String) ()
doComputerMove = do
  gameState <- get
  case nextPlayer gameState of
    Human -> lift $ Left "It is not the computer's turn."
    Computer -> do
      case selectNextMove (computerMark gameState) (gameBoard gameState) of
        Left errMsg -> lift $ Left $ "Cannot execute computer move: " ++ errMsg
        Right cell -> performMove cell

{-|
  Return a StateT that will either play the correct Mark (X or O) into the
  given Cell, or will return an error message.
-}
performMove :: Cell -> StateT GameState (Either String) ()
performMove cell = do
  gs <- get
  let player = nextPlayer gs
      mark = if player == Computer then (computerMark gs) else (humanMark gs)
  case fillCell cell mark (gameBoard gs) of
    Left errMsg    -> lift $ Left errMsg
    Right newBoard -> put gs { nextPlayer = flipPlayer player
                             , gameBoard = newBoard }

-- |Print the GameState to the screen in a convenient way.
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

-- |The main loop for the text version of this game.
textGameLoop :: GameState -> IO ()
textGameLoop gs = do
  printGameState gs
  case (checkGSForOutcome gs, nextPlayer gs) of
    (Just (Winner winner), _)   -> return ()
    (Just Draw, _)              -> return ()
    (Nothing, Computer)         -> do
      case execStateT doComputerMove gs of
        Left errMsg -> putStrLn errMsg
        Right newGS -> textGameLoop newGS
    (Nothing, Human)            -> do
      cell <- askWhichCell (gameBoard gs)
      case execStateT (doHumanMove cell) gs of
        Left errMsg -> putStrLn errMsg
        Right newGS -> textGameLoop newGS

-- |Entrypoint for running the text version of this game.
playTextGame :: IO ()
playTextGame = do
  humanMark <- askWhichMark
  textGameLoop (initGameState humanMark)