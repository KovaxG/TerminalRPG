{-# LANGUAGE NamedFieldPuns #-}
module Update (
  update
) where

import qualified Data.Map as Map
import           Data.Maybe (isNothing)
import           Debug.Trace

import Common

update :: Input -> World -> World
update input world@World { textField, entities, player, facing, settings } =
  case input of
    (Move direction) ->
      if isNothing textField
      then movePlayer direction world
      else world
    Interact ->
      if isNothing textField
      then let entity = getEntity facing player entities >>= messageOfEntity settings
           in setTextField entity world
      else cycleTextField world


getEntity :: Direction -> Position -> Store Entity -> Maybe Entity
getEntity direction player entities = orElse inFrontOfPlayer underPlayer
  where
    inFrontOfPlayer = get entities $ translate direction player
    underPlayer = get entities player

movePlayer :: Direction -> World -> World
movePlayer direction world@World { bounds, tiles, player } =
  let playerPosition' = snapBounds bounds $ translate direction player
  in if canGoTo tiles playerPosition'
     then world { facing = direction , player = playerPosition' }
     else world { facing = direction }

cycleTextField :: World -> World
cycleTextField w@World{textField=(Just tf@TextField{current,total})}
  | current + 1 > total = clearTextField w
  | otherwise = w { textField = Just $ tf { current = current + 1 } }

messageOfEntity :: Settings -> Entity -> Maybe TextField
messageOfEntity settings (Entity name msg) = toText settings $ name ++ ": " ++ msg

canGoTo :: Store Tile -> Position -> Bool
canGoTo tiles = maybe False isPassable . get tiles
