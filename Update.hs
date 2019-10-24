{-# LANGUAGE NamedFieldPuns #-}
module Update (
  update
) where

import           Data.List
import qualified Data.Map as Map
import           Data.Maybe (isNothing)
import           Debug.Trace

import Common

update :: Input -> World -> World
update input world@World {
    currentLevel=level@Level{ entities, player, facing },
    textField,
    settings,
    triggers
  } =
  case input of
    (Move direction) ->
      let level' = if isNothing textField
                   then movePlayer direction level
                   else level
      in world { currentLevel = level' }
    Interact ->
      if isNothing textField
      then let (tf, ts) = maybe (Nothing, triggers)
                                (effectsOfEntity settings triggers)
                                $ getEntity facing player entities
           in world { textField = tf, triggers = ts }
      else cycleTextField world


getEntity :: Direction -> Position -> Store Entity -> Maybe Entity
getEntity direction player entities = orElse inFrontOfPlayer underPlayer
  where
    inFrontOfPlayer = get entities $ translate direction player
    underPlayer = get entities player

movePlayer :: Direction -> Level -> Level
movePlayer direction level@Level{ bounds, tiles, player } =
  let playerPosition' = snapBounds bounds $ translate direction player
  in if canGoTo tiles playerPosition'
     then level { facing = direction, player = playerPosition' }
     else level { facing = direction }

cycleTextField :: World -> World
cycleTextField w@World{textField=(Just tf@TextField{current,total})}
  | current + 1 > total = clearTextField w
  | otherwise = w { textField = Just $ tf { current = current + 1 } }

effectsOfEntity :: Settings -> [Trigger] -> Entity -> (Maybe TextField, [Trigger])
effectsOfEntity settings ts (Entity name rs _) =
  maybe (Nothing, ts)
        (\(EntityEffect msg td) ->
          (toText settings $ name ++ ": " ++ msg, applyDiff ts td))
        $ fmap snd
        $ find (checkRules ts . fst) rs

canGoTo :: Store Tile -> Position -> Bool
canGoTo tiles = maybe False isPassable . get tiles
