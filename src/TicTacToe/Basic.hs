module TicTacToe.Basic
       ( -- * Data types
         Cell (..)
       , GameOutcome (..)
       , GameState (..)
       , Mark (..)
       , MoveError (..)
       , Player (..)

         -- * Type aliases
       , Board
       , TicTacToe
       , TicTacToeIO

        -- * Functions
       , checkGSForOutcome
       , emptyCells
       , evalTicTacToeIO
       , execTicTacToeIO
       , fillCell
       , flipMark
       , flipPlayer
       , getAllLineMarks
       , getWinningLines
       , humanMark
       , initGameState
       , liftTicTacToe
       , newGameState
       , nextMark
       , promoteMoveError
       , showBoard
       , performMove
       , doHumanMove
       , makeSampleGS
       ) where

import Control.Monad.Except (ExceptT, runExceptT, throwError, lift)
import Control.Monad.State
  (StateT, runStateT, evalStateT, execStateT, put, get)
import Data.Map (Map)
import Data.Maybe (isJust)
import System.Random (StdGen, mkStdGen)

import qualified Data.Map as Map


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

{-| A blank tic-tac-toe board, implemented as an empty (Map Cell Mark).
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
lineToCells :: Line -> (Cell, Cell, Cell)
lineToCells Line00to02 = (Cell00, Cell01, Cell02)
lineToCells Line10to12 = (Cell10, Cell11, Cell12)
lineToCells Line20to22 = (Cell20, Cell21, Cell22)
lineToCells Line00to20 = (Cell00, Cell10, Cell20)
lineToCells Line01to21 = (Cell01, Cell11, Cell21)
lineToCells Line02to22 = (Cell02, Cell12, Cell22)
lineToCells Line00to22 = (Cell00, Cell11, Cell22)
lineToCells Line20to02 = (Cell20, Cell11, Cell02)

{-| A data type that is returned when a move cannot be completed.  Although the
'a' is fully polymorphic, this type will probably only be used as 'MoveError
Mark' or 'MoveError Player'.
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
  show (CellFull cell mark) =
    concat [ showMoveErrorPrefix
           , show cell ++ " already contains " ++ show mark
           ]
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

{-| As the names suggest, this monad is similar to the TicTacToeIO monad, but
without IO.

StateT is used to handle the tic-tac-toe GameState.  The GameState also holds
the StdGen that is used by the Computer player to select a random best move
from the available moves. Either is used to return a MoveError if one occurs.
-}
type TicTacToe a = StateT GameState (Either (MoveError Player)) a


{-| This monad is similar to the TicTacToe monad, but
with IO. This monad is used for the playable text version of the game.

StateT is used to handle the tic-tac-toe GameState. The GameState also holds
the StdGen that is used by the Computer player to select a random best move
from the available moves. ExceptT is used to return a MoveError if one occurs.
-}
type TicTacToeIO a = StateT GameState (ExceptT (MoveError Player) IO) a

-- | Turn a TicTacToe operation into a TicTacToeIO operation.
liftTicTacToe :: TicTacToe a -> TicTacToeIO a
liftTicTacToe f = do
  gs <- get
  case runStateT f gs of
    Left err -> lift $ throwError err
    Right (a, gs') -> put gs' >> pure a

{-| Run a TicTacToeIO operation against a GameState and get the resulting
GameState as the return value.
-}
execTicTacToeIO :: TicTacToeIO a
              -> GameState
              -> IO (Either (MoveError Player) GameState)
execTicTacToeIO tttIO = runExceptT . execStateT tttIO

{-| Run a TicTacToeIO operation against a GameState and get back the result of
that operation.
-}
evalTicTacToeIO :: TicTacToeIO a
               -> GameState
               -> IO (Either (MoveError Player) a)
evalTicTacToeIO tttIO = runExceptT . evalStateT tttIO

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
showCellContents :: Board -> Cell -> String
showCellContents board cell =
  case Map.lookup cell board of
    Nothing   -> " "
    Just mark -> show mark

-- | Return the String representation of a horizontal line of the Board
showLine :: Board -> Line -> String
showLine board line =
  let (c1, c2, c3) = lineToCells line
   in concat [ "  "
             , showCellContents board c1
             , " | "
             , showCellContents board c2
             , " | "
             , showCellContents board c3
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

instance Eq GameState where
  GameState _ p1 m1 b1 == GameState _ p2 m2 b2 =
    p1 == p2 && m1 == m2 && b1 == b2

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
