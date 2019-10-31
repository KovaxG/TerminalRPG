{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
module Convenience where

import qualified Data.Map as Map
import           Data.List (groupBy)

import Common

draw :: World -> String
draw World{currentLevel=level@Level{player, facing, bounds, tiles, entities}, textField, triggers, settings} =
  unlines [
    drawDebug triggers,
    drawPlayerInfo facing player,
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


drawPlayerInfo :: Direction -> Position -> String
drawPlayerInfo dir pos = show pos ++ ", Facing: " ++ case dir of
  North -> "^"
  South -> "v"
  West -> "<"
  East -> ">"


drawTextField :: Settings -> TextField -> String
drawTextField Settings{textBoxWidth} TextField{text, current} =
  surroundWithBox
    $ init
    $ unlines
    $ map addPadding
    $ text !! (current-1)
  where
    addPadding :: String -> String
    addPadding l = l ++ replicate (textBoxWidth - 1 - length l) ' '


surroundWithBox :: String -> String
surroundWithBox t =
  unlines $ horLine "┌" "┐" ++ map (\l -> "│" ++ l ++ "│") ls ++ horLine "└" "┘"
  where
    ls = lines t
    horLine a b = [a ++ replicate (length $ head ls) '─' ++ b]


drawMap :: Level -> String
drawMap Level{name=(LevelName name), tiles, bounds = (RB sx sy ex ey), entities = (Store es), player} =
  (++) ("\n\n" ++ "Level: " ++ name ++ "\n")
    $ surroundWithBox
    $ unlines
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
    surround l = "│" ++ l ++ "│"

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
      InteriorWindow -> "─"
      WindowedWall   -> "▄"
      RightSlope     -> "\\"
      BoxTileULC     -> "┌"
      BoxTileURC     -> "┐"
      BoxTileLLC     -> "└"
      BoxTileLRC     -> "┘"
      CornerULSD     -> "╒"
      CornerURSD     -> "╕"
      BoxTileLS      -> "├"
      BoxTileRS      -> "┤"
      LeftSlope      -> "/"
      HorBorder      -> "_"
      DHorLine       -> "═"
      VerLine        -> "│"
      Grass          -> " "
      Wall           -> "█"
      Void           -> "X"

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
        toTile c = case c of
          '─'  -> Tile { isPassable = False, tileBase = InteriorWindow }
          '▄'  -> Tile { isPassable = False, tileBase = WindowedWall   }
          '\\' -> Tile { isPassable = False, tileBase = RightSlope     }
          '┌'  -> Tile { isPassable = False, tileBase = BoxTileULC     }
          '┐'  -> Tile { isPassable = False, tileBase = BoxTileURC     }
          '└'  -> Tile { isPassable = False, tileBase = BoxTileLLC     }
          '┘'  -> Tile { isPassable = False, tileBase = BoxTileLRC     }
          '╒'  -> Tile { isPassable = False, tileBase = CornerULSD     }
          '╕'  -> Tile { isPassable = False, tileBase = CornerURSD     }
          '├'  -> Tile { isPassable = False, tileBase = BoxTileLS      }
          '┤'  -> Tile { isPassable = False, tileBase = BoxTileRS      }
          '/'  -> Tile { isPassable = False, tileBase = LeftSlope      }
          '_'  -> Tile { isPassable = False, tileBase = HorBorder      }
          '═'  -> Tile { isPassable = False, tileBase = DHorLine       }
          '│'  -> Tile { isPassable = False, tileBase = VerLine        }
          ' '  -> Tile { isPassable = True,  tileBase = Grass          }
          '█'  -> Tile { isPassable = False, tileBase = Wall           }
          _    -> voidTile

horBox :: Int -> String
horBox len = "+" ++ replicate len '-' ++ "+"
