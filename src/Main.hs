module Main where

import Control.Monad.State (get, liftIO)
import Data.Char (toUpper)

import TicTacToe.AI (doComputerMove)
import TicTacToe.Basic (TicTacToeIO, Board, Cell, Player(..), GameState(..),
                        Mark, GameOutcome(..), doHumanMove, humanMark,
                        showBoard, emptyCells, checkGSForOutcome,
                        liftTicTacToe, initGameState, evalTicTacToeIO)


main :: IO ()
main = do
  mark <- askWhichMark
  moveErrorOrUnit <- evalTicTacToeIO textGameLoop (initGameState mark 42)
  case moveErrorOrUnit of
    Left err -> print err
    _        -> pure ()

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
    (mark, _):_ -> return mark

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
