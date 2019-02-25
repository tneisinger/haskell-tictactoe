module Main where

import Control.Monad.State (get, liftIO)
import Data.Char (toUpper)
import System.IO (hFlush, stdout)

import TicTacToe.Exports (Cell, GameOutcome(..),
                          GameState(gameBoard, nextPlayer), GameOutcome(..),
                          Mark, Player(..), TicTacToeIO, checkGSForOutcome,
                          doComputerMove, doHumanMove, emptyCells,
                          evalTicTacToeIO, humanMark, initGameState,
                          liftTicTacToe, maybeIntToCell, showBoard,
                          showBoardWithCellNums)


main :: IO ()
main = do
  putStrLn ""
  mark <- askWhichMark
  putStrLn ""
  moveErrorOrUnit <- evalTicTacToeIO textGameLoop (initGameState mark 42)
  case moveErrorOrUnit of
    Left err -> print err
    _        -> pure ()

-- | The main loop for the text version of this game.
textGameLoop :: TicTacToeIO ()
textGameLoop = do
  gs <- get
  case (checkGSForOutcome gs, nextPlayer gs) of
    (Just _, _)         -> liftIO $
      printGameMessage gs >> putStrLn (showBoard (gameBoard gs))
    (Nothing, Computer) -> liftTicTacToe doComputerMove >> textGameLoop
    (Nothing, Human)    -> do
      liftIO $ printGameMessage gs
      cell <- liftIO $ askWhichCell gs
      liftTicTacToe $ doHumanMove cell
      textGameLoop

{-| Ask the human which Mark they would like to play as.  The human's response
will determine if the human or the computer will move first, since X always
moves first.
-}
askWhichMark :: IO Mark
askWhichMark = do
  response <- prompt "Would you like to play as X or O? "
  case reads (map toUpper response) of
    [] -> putStrLn "Please enter an X or an O..." >> askWhichMark
    (mark, _):_ -> return mark

{-| Ask the player which Cell they would like to play into.  Return the Cell
chosen by the player.
-}
askWhichCell :: GameState -> IO Cell
askWhichCell gs = do
  let choices = emptyCells (gameBoard gs)
      boardStr = showBoard (gameBoard gs)
      boardWithNumsStr = showBoardWithCellNums (gameBoard gs)
  putStrLn "Enter a number (1-9) to select your move:"
  putStrLn $ sideBySideStrings boardStr 4 boardWithNumsStr
  intStr <- prompt "> "
  case maybeReadToCell intStr of
    Nothing -> putStrLn "Invalid input. Try again." >> askWhichCell gs
    Just cell ->
      if cell `notElem` choices
         then putStrLn "That cell is full. Try again" >> askWhichCell gs
         else putStrLn "" >> return cell

{-| Maybe read a String into a Cell.  The String is expected to be an integer
between 1 and 9 inclusive.  If it is not, return Nothing.
-}
maybeReadToCell :: String -> Maybe Cell
maybeReadToCell intStr =
  case reads intStr of
    [] -> Nothing
    ((int, _):_) -> maybeIntToCell int

-- | Print the GameState to the screen in a convenient way.
printGameMessage :: GameState -> IO ()
printGameMessage gs =
  case checkGSForOutcome gs of
    Nothing -> putStrLn $ "It is your turn.  You are " ++ show (humanMark gs)
    Just (Winner winner) -> putStrLn $ show winner ++ " wins!"
    Just Draw -> putStrLn "Draw! Nobody wins :("

-- | Prompt the user for an input.
prompt :: String -> IO String
prompt text = do
  putStr text
  hFlush stdout
  getLine

{-| Combine two Strings into one String so that when the resulting String is
printed, the output looks like the two input Strings printed side-by-side. The
'numSpaces' Int determines the number of spaces by which to separate them.
-}
sideBySideStrings :: String -> Int -> String -> String
sideBySideStrings str1 numSpaces str2 =
    unlines $ zipWith go lines1 lines2
  where (lines1, lines2) = (lines str1, lines str2)
        go l1 l2 = l1 ++ replicate numSpaces ' ' ++ l2
