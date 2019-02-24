module Main where

import TicTacToe

main :: IO ()
main = do
  case getBestCells (initGameState O 42) of
    Left _ -> putStrLn "An error occurred"
    Right cells -> do
      putStrLn "Cells:"
      sequence_ $ map print cells

