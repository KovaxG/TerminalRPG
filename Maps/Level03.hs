{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Maps.Level03 (level03) where

import Common
import Convenience

level03 :: Level
level03 =
  Level {
    name = LevelName "Bait",
    bounds,
    tiles,
    entities = asStore ents,
    teleports = asStore tels,
    player = newPos 0 0,
    facing = South
  }
  where
    (tiles, bounds) = parseMap theMap

theMap = [
  "                                                   _                          ",
  "                                                  /_\\                         ",
  "                                                  █ █                         ",
  "                                                                              ",
  "                                                             ____             ",
  "                                                             █ ██             ",
  "                                         ___                                  ",
  "                                    ____/   \\____                             ",
  "                                   /    █████    \\                            ",
  "                                   ██████▄█▄██████                            ",
  "        ┌┐                         █▄█▄█▄█ █▄█▄█▄█                            ",
  "      ┌┐└┘┌┐   _                                                              ",
  "      └┘  └┘  /_\\                                                             ",
  "      ┌┐      █ █                                                             ",
  "      └┘  ┌┐                                               _  __  _           ",
  "      ┌┐  ├┤                                              /_\\/__\\/_\\          ",
  "      └┘  └┘                                              █▄██▄▄██▄█          ",
  "                                                          █▄█▄██▄█▄█          ",
  "                                                          █▄▄█  █▄▄█          ",
  "                         ___                                                  ",
  "                        /   \\____                                             ",
  "                        █████    \\                                            ",
  "                        █▄█▄██████              _____                         ",
  "                        ██ ███▄█▄█             /     \\                        ",
  "                                               ███████                        ",
  "                                               █▄█▄█▄█                        ",
  "                                               █▄█ █▄█                        ",
  "   ___                                                                        ",
  "  /   \\                                                                       ",
  "  █████                                                                       ",
  "  █ █▄█                                                                       ",
  "                                                                              ",
  "                                                                              ",
  "                                               ╒══╕ ╒══╕                      ",
  "         _____                                 │  │ │  │                      ",
  "        /     \\                                          ╒══╕                 ",
  "        ███████                                          │  │                 ",
  "        █▄█ █▄█                                   ╒══╕                        ",
  "                                                  │  │                        ",
  "                                                                              "
  ]

ents = [
    (P 6 2, Entity {
      name = "Secret",
      effects = [(Exists (Trigger "test"), EntityEffect "You found me! Good job my dude." $ RemoveTrigger (Trigger "test"))],
      sprite = Sprite 'X'
      })
  ]

tels = []
