{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Maps.Level01 (level01) where

import Common
import Convenience

level01 :: Level
level01 =
  Level {
    name = LevelName "Test",
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
  "                 ",
  "   █████         ",
  "   █   █         ",
  "   ██ ██         ",
  "                 ",
  "                 "
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

tels = [
  (P 0 0, Teleport $ LevelName "Wall")
  ]
