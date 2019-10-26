{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Maps.Level02 (level02) where

import Common
import Convenience

level02 :: Level
level02 =
  Level {
    name = LevelName "Wall",
    bounds,
    tiles,
    entities = asStore ents,
    teleports = asStore tels,
    player = newPos 2 2,
    facing = South
  }
  where
    (tiles, bounds) = parseMap theMap

theMap = [
  "       █     ",
  "       █     ",
  "       █     ",
  "       █     ",
  "       █     ",
  "       █     "
  ]

ents = [
    (P 6 2, Entity {
      name = "Secret",
      effects = [(Exists (Trigger "test"), EntityEffect "You found me! Good job my dude." $ RemoveTrigger (Trigger "test"))],
      sprite = Sprite 'X'
      }),
    (P 14 1, Entity {
      name = "Second Secret",
      effects = [(Missing (Trigger "test"), EntityEffect "Here, you can find the first secret again!" $ AddTrigger (Trigger "test"))],
      sprite = Sprite 'S'
    })
  ]

tels = []
