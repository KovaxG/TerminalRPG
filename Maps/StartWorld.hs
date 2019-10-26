{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Maps.StartWorld where

import qualified Data.Map as Map
import Common

import Maps.Level01
import Maps.Level02

startWorld :: World
startWorld =
  World {
    levels,
    currentLevel = level01,
    textField = Nothing,
    settings,
    triggers = [Trigger "test"]
  }
  where
    levels = Map.fromList $ map (\l -> (name (l :: Level), l)) [level01, level02]
    settings = Settings { textBoxWidth = 30, textBoxHeight = 3 }
