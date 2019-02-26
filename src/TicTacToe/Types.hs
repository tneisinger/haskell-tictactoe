module TicTacToe.Types
       ( -- * Data types
         Cell (..)
       , GameOutcome (..)
       , GameState (..)
       , Line (..)
       , Mark (..)
       , MoveError (..)
       , Player (..)

         -- * Type aliases
       , Board
       , TicTacToe
       , TicTacToeIO

         -- * Functions
       , liftTicTacToe
       , evalTicTacToe
       , execTicTacToe
       , evalTicTacToeIO
       , execTicTacToeIO
       ) where

import Data.Map (Map)
import Control.Monad.Except (ExceptT, runExceptT, throwError)
import Control.Monad.State (StateT, evalStateT, execStateT, runStateT, put,
                            get, lift)
import System.Random (StdGen)

import qualified Data.Map as Map


-- | A simple datatype to represent an X or an O in a tic-tac-toe game.
data Mark = X | O
  deriving (Eq, Show, Read)

-- | A type to represent the 9 cells of a tic-tac-toe board.
data Cell = Cell00 | Cell01 | Cell02
          | Cell10 | Cell11 | Cell12
          | Cell20 | Cell21 | Cell22
          deriving (Eq, Show, Enum, Ord, Read)

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
           , show (Map.toList board) ++ "\n\n"
           ]

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

-- | A simple Type for the players of the game.
data Player = Computer | Human
  deriving (Eq, Show)

-- | The State of a tic-tac-toe game.
data GameState = GameState { stdGen :: StdGen
                           , nextPlayer :: Player
                           , computerMark :: Mark
                           , gameBoard :: Board
                           } deriving (Show)

instance Eq GameState where
  GameState _ p1 m1 b1 == GameState _ p2 m2 b2 =
    p1 == p2 && m1 == m2 && b1 == b2

{-| A Type representing the outcome of a game. This will be used as either
'GameOutcome Mark' or 'GameOutcome Player'.
-}
data GameOutcome a = Winner a | Draw
  deriving (Eq, Show)

{-| A Board is a Map from a Cell to a Mark.  If a Cell is not in the Map,
that corresponds to that cell being empty.
-}
type Board = (Map Cell Mark)

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

{-| Run a TicTacToe operation against a GameState and get the resulting
GameState as the return value.
-}
execTicTacToe :: TicTacToe a
              -> GameState
              -> Either (MoveError Player) GameState
execTicTacToe t = execStateT t

{-| Run a TicTacToe operation against a GameState and get back the result of
that operation.
-}
evalTicTacToe :: TicTacToe a
               -> GameState
               -> Either (MoveError Player) a
evalTicTacToe t = evalStateT t


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

