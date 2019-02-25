module TicTacToe.Exports
       ( -- * Types
         Board
       , Cell(..)
       , GameOutcome(..)
       , GameState(computerMark, gameBoard, nextPlayer)
       , Mark(..)
       , Player(..)
       , TicTacToe
       , TicTacToeIO

         -- * Functions
       , checkGSForOutcome
       , doComputerMove
       , doHumanMove
       , emptyCells
       , evalTicTacToeIO
       , humanMark
       , initGameState
       , liftTicTacToe
       , newGameState
       , showBoard
       , showCellContents
       ) where

import TicTacToe.AI (doComputerMove)
import TicTacToe.Basic (checkGSForOutcome, doHumanMove, emptyCells, humanMark,
                        initGameState, newGameState, showBoard,
                        showCellContents)
import TicTacToe.Types (Board, Cell(..), GameOutcome(..),
                        GameState(nextPlayer, computerMark, gameBoard),
                        Mark(..), Player(..), TicTacToe, TicTacToeIO,
                        evalTicTacToeIO, liftTicTacToe)
