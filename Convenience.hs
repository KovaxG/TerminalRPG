{-# LANGUAGE NamedFieldPuns #-}
module Convenience where

import qualified Data.Map as Map
import           Data.List (groupBy)

import Common

draw :: World -> String
draw World{player, facing, bounds, tiles, entities, textField, settings} =
  unlines [
    drawFacing facing,
    drawMap tiles bounds entities player,
    maybe "" (drawTextField settings) textField
    ]

drawFacing :: Direction -> String
drawFacing dir = "Facing: " ++ case dir of
  North -> "^"
  South -> "v"
  West -> "<"
  East -> ">"

drawTextField :: Settings -> TextField -> String
drawTextField Settings{textBoxWidth} TextField{text, current} =
  unlines ["\n", horizontal, text', horizontal, "\n"]
  where
    horizontal :: String
    horizontal = horBox (textBoxWidth-1)

    text' :: String
    text' = init
            $ unlines
            $ map surroundWithPadding
            $ text !! (current-1)

    surroundWithPadding :: String -> String
    surroundWithPadding l =
      "|" ++ l ++ replicate (textBoxWidth - 1 - length l) ' ' ++ "|"

drawMap :: Store Tile -> Bounds -> Store Entity -> Position -> String
drawMap ts (RB sx sy ex ey) (Store es) player =
  (++) "\n\n"
    $ unlines
    $ addUpperLowerBound
    $ map surround
    $ map (
        (=<<) (insertPlayerOrEntity (maybe " " toString . get ts))
      )
    $ groupBy rows
    $ [P x y | y <- [sy .. ey], x <- [sx .. ex]]
  where
    addUpperLowerBound :: [String] -> [String]
    addUpperLowerBound a = horLine ++ a ++ horLine
      where horLine = [horBox (ex - sx + 1)]


    surround :: String -> String
    surround l = "|" ++ l ++ "|"

    insertPlayerOrEntity :: (Position -> String) -> Position -> String
    insertPlayerOrEntity f p
      | p == player = "P"
      | elem p entityPositions = "E"
      | otherwise = f p
      where
        entityPositions = map fst $ Map.toList es

    rows :: Position -> Position -> Bool
    rows (P _ x) (P _ y) = x == y

    toString :: Tile -> String
    toString Tile {tileBase} = case tileBase of
      Grass -> " "
      Wall -> "█"
      Void -> "X"

parseMap :: [String] -> (Store Tile, Bounds)
parseMap = toMap . process
  where
    process :: [String] -> (String, Int, Int)
    process raw = (concat raw, length $ head raw, length raw)

    toMap :: (String, Int, Int) -> (Store Tile, Bounds)
    toMap (raw, w, h) =
      (\s -> (s, newBounds (w-1) (h-1)))
        $ Store
        $ Map.fromList
        $ zip [P x y | y <- [0 .. h-1], x <- [0 .. w-1]]
        $ map toTile raw
      where
        toTile :: Char -> Tile
        toTile ' ' = Tile { isPassable = True, tileBase = Grass }
        toTile '█' = Tile { isPassable = False, tileBase = Wall }
        toTile 'X' = voidTile


myMap :: (Store Tile, Bounds, Store Entity)
myMap = addExtra entities $ parseMap [
  "                 ",
  "   █████         ",
  "   █   █         ",
  "   ██ ██         ",
  "                 ",
  "                 "
  ]
  where
    addExtra :: c -> (a,b) -> (a,b,c)
    addExtra c (a,b) = (a,b,c)

    entities :: Store Entity
    entities = Store $ Map.fromList [
        (P 6 2, Entity "Secret" "You found me! Now try to implement the thing that would make me speak only when you are next to me, not stepping on me, since that crap kind of hurts and I don't appreaciate it at all.")
      ]


startWorld :: World
startWorld =
  World {
    bounds,
    tiles,
    player = newPos 2 2,
    facing = South,
    textField = toText settings "Hello, my name is Gyuri, what is going on with you? Are you enjoying your new job? I sure hope you are.",
    entities,
    settings
  }
  where
    (tiles, bounds, entities) = myMap
    settings = Settings { textBoxWidth = 30, textBoxHeight = 3 }


horBox :: Int -> String
horBox len = "+" ++ replicate len '-' ++ "+"
