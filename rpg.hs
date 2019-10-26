{-# LANGUAGE NamedFieldPuns #-}

import System.Process

import Common
import Convenience
import Maps.StartWorld
import Update


main :: IO ()
main = do
  putStrLn "Starting Game"
  playGame


playGame :: IO ()
playGame = loop startWorld
  where
    loop :: World -> IO ()
    loop world = do
      system "cls"
      putStrLn $ draw world
      input <- getInput
      let world' = update input world
      loop world'

    getInput :: IO Input
    getInput = do
      string <- getLine
      maybe (putStrLn "Invalid input!" >> getInput)
            return
            (parseInput string)

    parseInput :: String -> Maybe Input
    parseInput string = case string of
      'w':_ -> Just $ Move North
      's':_ -> Just $ Move South
      'a':_ -> Just $ Move West
      'd':_ -> Just $ Move East
      ' ':_ -> Just $ Interact
      "" -> Just $ Interact
      _ -> Nothing
