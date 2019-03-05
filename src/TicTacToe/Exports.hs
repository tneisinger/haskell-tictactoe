module TicTacToe.Exports
       ( -- * Types
         Board
       , Cell(..)
       , Difficulty(..)
       , GameOutcome(..)
       , GameState(computerMark, gameBoard, nextPlayer)
       , Mark(..)
       , Player(..)
       , TicTacToe
       , TicTacToeIO

         -- * Functions
       , allDifficulties
       , checkForOutcome
       , doComputerMove
       , doHumanMove
       , emptyCells
       , evalTicTacToe
       , evalTicTacToeIO
       , execTicTacToe
       , getWinningCells
       , getWinningLines
       , humanMark
       , initGameState
       , liftTicTacToe
       , makeSampleGS
       , maybeIntToCell
       , newGameState
       , showBoard
       , showBoardWithCellNums
       , showCellContents
       ) where

import TicTacToe.AI (doComputerMove)
import TicTacToe.Basic (checkForOutcome, doHumanMove, emptyCells,
                        getWinningCells, getWinningLines, humanMark,
                        initGameState, makeSampleGS, maybeIntToCell,
                        newGameState, showBoard, showBoardWithCellNums,
                        showCellContents)
import TicTacToe.Types (Board, Cell(..), Difficulty(..), GameOutcome(..),
                        GameState(nextPlayer, computerMark, gameBoard),
                        Mark(..), Player(..), TicTacToe, TicTacToeIO,
                        allDifficulties, evalTicTacToe, evalTicTacToeIO,
                        execTicTacToe, liftTicTacToe)
