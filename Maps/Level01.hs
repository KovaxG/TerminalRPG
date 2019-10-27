{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Maps.Level01 (level01) where

import Common
import Convenience

level01 :: Level
level01 =
  Level {
    name = LevelName "Exterior",
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
  "                 ",
  "   ____          ",
  "  /____\\         ",
  "  ██████         ",
  "  ██ █▄█         ",
  "                 ",
  "                 ",
  "                 "
  ]

ents = [
    (P 14 1, Entity {
      name = "Second Secret",
      effects = [(Missing (Trigger "test"), EntityEffect "Here, you can find the first secret again!" $ AddTrigger (Trigger "test"))],
      sprite = Sprite 'S'
    })
  ]

tels = [
  (P 4 5, Teleport $ LevelName "Interior")
  ]
