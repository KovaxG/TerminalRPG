{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Common where

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)


data World = World {
  levels :: Map.Map LevelName Level,
  currentLevel :: Level,
  textField :: Maybe TextField,
  settings :: Settings,
  triggers :: [Trigger]
} deriving (Show)

data Level = Level {
  name :: LevelName,
  bounds :: Bounds,
  tiles :: Store Tile,
  entities :: Store Entity,
  teleports :: Store Teleport,
  player :: Position,
  facing :: Direction
} deriving (Show)

newtype LevelName = LevelName String deriving (Show, Ord, Eq)

data Teleport = Teleport {
  targetMap :: LevelName
} deriving (Show)

data Direction = North | South | East | West deriving (Show)

data Input = Move Direction | Interact deriving (Show)

data Position = P Int Int deriving (Show, Eq, Ord)

data WorldMap = WorldMap  (Store Tile) deriving (Show)

data Bounds = RB Int Int Int Int deriving (Show)

data TileBase = Void
              | Wall
              | Grass
              | HorBorder
              | LeftSlope
              | RightSlope
              | WindowedWall
              | InteriorWindow
              deriving (Show)

data Trigger = Trigger String deriving (Show, Eq)

data EntityEffect = EntityEffect String TriggerDiff deriving (Show)

data TriggerDiff = AddTrigger Trigger | RemoveTrigger Trigger deriving (Show)

data TriggerCondition = Missing Trigger | Exists Trigger | Always deriving (Show)

data Store a = Store (Map.Map Position a) deriving (Show)

data Sprite = Sprite Char deriving (Show)

data Tile = Tile {
  isPassable :: Bool,
  tileBase :: TileBase
} deriving (Show)

data Settings = Settings {
  textBoxWidth :: Int,
  textBoxHeight :: Int
} deriving (Show)

data TextField = TextField {
  text :: [[String]],
  total :: Int,
  current :: Int
} deriving (Show)

data Entity = Entity {
  name :: String,
  effects :: [(TriggerCondition, EntityEffect)],
  sprite :: Sprite
} deriving (Show)

toText :: Settings -> String -> Maybe TextField
toText Settings{textBoxWidth,textBoxHeight} str =
  Just $ TextField { text, current = 1, total }
  where
    text :: [[String]]
    text = chunksOf textBoxHeight $ chunkUpText str

    total :: Int
    total = length text

    chunkUpText :: String -> [String]
    chunkUpText = reverse . foldl rule [] . words
      where
        rule :: [String] -> String -> [String]
        rule [] w = [w]
        rule (l:ls) w
          | length (l ++ " " ++ w) < textBoxWidth = (l ++ " " ++ w):ls
          | otherwise = w:l:ls

applyDiff :: [Trigger] -> TriggerDiff -> [Trigger]
applyDiff ts (AddTrigger t)
  | elem t ts = ts
  | otherwise = t : ts
applyDiff ts (RemoveTrigger t) = filter (/=t) ts

checkRules :: [Trigger] -> TriggerCondition -> Bool
checkRules ts (Exists t) = elem t ts
checkRules ts (Missing t) = not $ elem t ts
checkRules _ Always = True

voidTile :: Tile
voidTile = Tile { isPassable = False, tileBase = Void }

newBounds :: Int -> Int -> Bounds
newBounds x y = RB 0 0 x y

newPos :: Int -> Int -> Position
newPos = P

translate :: Direction -> Position -> Position
translate North (P x y) = P x (y-1)
translate South (P x y) = P x (y+1)
translate East  (P x y) = P (x+1) y
translate West  (P x y) = P (x-1) y

snapBounds :: Bounds -> Position -> Position
snapBounds (RB sx sy ex ey) (P x y) = P (snap sx ex x) (snap sy ey y)
  where
    snap :: Int -> Int -> Int -> Int
    snap l u v
      | u < v = u
      | v < l = l
      | otherwise = v

chunksOf :: Int -> [e] -> [[e]]
chunksOf i ls = map (take i) (build (splitter ls))
  where
    splitter :: [e] -> ([e] -> a -> a) -> a -> a
    splitter [] _ n = n
    splitter l c n  = l `c` splitter (drop i l) c n

    build :: ((a -> [a] -> [a]) -> [a] -> [a]) -> [a]
    build g = g (:) []

clearTextField :: World -> World
clearTextField w = w { textField = Nothing }

asStore :: [(Position, a)] -> Store a
asStore = Store . Map.fromList

get :: Store a -> Position -> Maybe a
get (Store innerMap) = flip Map.lookup innerMap

orElse :: Maybe a -> Maybe a -> Maybe a
orElse ma mb = maybe mb Just ma
