{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Maps.Level02 (level02) where

import Common
import Convenience

level02 :: Level
level02 =
  Level {
    name = LevelName "Interior",
    bounds,
    tiles,
    entities = asStore ents,
    teleports = asStore tels,
    player = newPos 6 4,
    facing = South
  }
  where
    (tiles, bounds) = parseMap theMap

theMap = [
  "█████████████",
  "█           █",
  "█           █",
  "█           █",
  "█           █",
  "██████ ██──██"
  ]

ents = [
    (P 6 2, Entity {
      name = "Secret",
      effects = [(Exists (Trigger "test"), EntityEffect "You found me! Good job my dude." $ RemoveTrigger (Trigger "test"))],
      sprite = Sprite 'X'
      })
  ]

tels = [
  (P 6 5, Teleport { targetMap = LevelName "Exterior", targetPosition = P 4 6 })
  ]
