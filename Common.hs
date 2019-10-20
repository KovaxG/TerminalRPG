{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Common where

import qualified Data.Map as Map
import           Data.Maybe (fromMaybe)


data World = World {
  bounds :: Bounds,
  tiles :: Store Tile,
  entities :: Store Entity,
  player :: Position,
  facing :: Direction,
  textField :: Maybe TextField,
  settings :: Settings
} deriving (Show)

data Direction = North | South | East | West deriving (Show)

data Input = Move Direction
           | Interact
           deriving (Show)

data Position = P Int Int deriving (Show, Eq, Ord)

data WorldMap = WorldMap  (Store Tile) deriving (Show)

data Bounds = RB Int Int Int Int deriving (Show)

data TileBase = Void | Grass | Wall deriving (Show)

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

data Entity = Entity {
  name :: String,
  message :: String
} deriving (Show)

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

setTextField :: Maybe TextField -> World -> World
setTextField mtf w = w { textField = mtf }

clearTextField :: World -> World
clearTextField w = w { textField = Nothing }

data Store a = Store (Map.Map Position a) deriving (Show)

get :: Store a -> Position -> Maybe a
get (Store innerMap) = flip Map.lookup innerMap

orElse :: Maybe a -> Maybe a -> Maybe a
orElse ma mb = maybe mb Just ma
