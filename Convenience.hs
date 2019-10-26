{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Convenience where

import qualified Data.Map as Map
import           Data.List (groupBy)

import Common

draw :: World -> String
draw World{currentLevel=level@Level{player, facing, bounds, tiles, entities}, textField, triggers, settings} =
  unlines [
    drawDebug triggers,
    drawFacing facing,
    drawMap level,
    maybe "" (drawTextField settings) textField
    ]

drawDebug :: [Trigger] -> String
drawDebug =
  (++) "Debug:\n"
  . (++) "Triggers: "
  . (++"\n")
  . unwords
  . map (\(Trigger name) -> name)


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


drawMap :: Level -> String
drawMap Level{name=(LevelName name), tiles, bounds = (RB sx sy ex ey), entities = (Store es), player} =
  (++) ("\n\n" ++ "Level: " ++ name ++ "\n")
    $ unlines
    $ addUpperLowerBound
    $ map surround
    $ map (
        (=<<) (insertPlayerOrEntity (maybe " " toString . get tiles))
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
      | otherwise = maybe (f p) (\(Sprite c) -> c:"") $ fmap sprite $ get (Store es) p
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

horBox :: Int -> String
horBox len = "+" ++ replicate len '-' ++ "+"
