{-# LANGUAGE NamedFieldPuns, DuplicateRecordFields #-}
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import System.Process

type Position = (Int, Int)
type Bounds = (Int, Int, Int, Int)
type AbsBounds = (Int, Int)

data Sprite  = Sprite {
  sName :: String,
  sBounds :: Bounds,
  sTiles :: Map.Map Position Char
} deriving (Show)

instance Eq Sprite where
  Sprite {sName=n1} == Sprite {sName=n2} = n1 == n2

data World = World {
  cursor :: Position,
  background :: Sprite,
  sprites :: [Sprite],
  selected :: Maybe Sprite
} deriving (Show)

data Command = Move Direction | Interact deriving (Show)
data Direction = North | South | East | West deriving (Show)


isOnTopOf :: Position -> Sprite -> Bool
isOnTopOf (x,y) Sprite { sBounds=(sx, sy, ex, ey) } =
  sx <= x && x <= ex && sy <= y && y <= ey

cursorToSprite :: Position -> Sprite
cursorToSprite (x,y) = Sprite {
  sName = "Cursor",
  sBounds = (x,y,x,y),
  sTiles = Map.fromList [((x,y), '^')]
}


moveTo :: Position -> Sprite -> Sprite
moveTo (x, y) s@Sprite { sBounds = (sx, sy, ex, ey), sTiles } =
  s {
    sBounds = (x, y, x + ex - sx, y + ey - sy),
    sTiles = Map.mapKeys (\(a,b) -> (a + x, b + y)) sTiles
  }

translate :: (Int, Int) -> Sprite -> Sprite
translate (dx, dy) s@Sprite { sBounds = (sx, sy, ex, ey) } =
  s { sBounds = (sx + dx, sy + dy, ex + dx, ey + dy) }


combineSprite :: Sprite -> Sprite -> Sprite
combineSprite Sprite { sName=n1, sBounds=b1@(sx1, sy1, ex1, ey1), sTiles=tiles1 }
              Sprite { sName=n2, sBounds=b2@(sx2, sy2, ex2, ey2), sTiles=tiles2 } =
  Sprite { sName = n1 ++ "+" ++ n2, sBounds = (sx, sy, ex, ey), sTiles = tiles }
  where
    (sx, sy, ex, ey) = (min sx1 sx2, min sy1 sy2, max ex1 ex2, max ey1 ey2)

    tiles :: Map.Map Position Char
    tiles =
      Map.fromList
      $ map (\p ->
        (,) p
        $ fromMaybe (fromMaybe ' ' $ Map.lookup p tiles2)
        $ Map.lookup p tiles1)
      $ [(x,y) | y <- [sx .. ex], x <- [sy .. ey]]



readSprite :: String -> String -> Sprite
readSprite name s = Sprite { sName = name, sTiles = tiles, sBounds = (0,0,ex,ey) }
  where
    (ex, ey) = maximum $ Map.keys tiles
    tiles = Map.fromList
            $ (=<<) (\(y, l) -> map (\(x, t) -> ((x, y), t)) $ zip [0 ..] l)
            $ zip [0 ..]
            $ lines s

drawSprite :: Sprite -> String
drawSprite Sprite { sTiles, sBounds = (_, _, ex, ey) } =
  unlines
  $ map (map (fromMaybe 'X' . flip Map.lookup sTiles))
  $ groupBy (\(_, a) (_, b) -> a == b)
  $ [(x,y) | y <- [0 .. ey], x <- [0 .. ex]]

draw :: World -> String
draw w@World {cursor, background, sprites, selected} =
  let newSprite = foldl (flip combineSprite) background $ sprites ++ [cursorToSprite cursor]
  in sName newSprite ++ "\n" ++ show selected ++ "\n" ++ drawSprite newSprite
  where
    upperBorder ls = ["┌" ++ replicate (length (head ls) - 2) '─' ++ "┐"]
    lowerBorder ls = ["└" ++ replicate (length (head ls) - 2) '─' ++ "┘"]

editor :: IO ()
editor = loop startWorld

startWorld :: World
startWorld = World { cursor = (0,0), background = blankSprite, sprites = [house], selected = Nothing }
  where blankSprite = readSprite "Background" $ concat $ replicate 10 "          \n"


update :: Command -> World -> World
update (Move dir) w@World{cursor, selected} =
  let nc = moveCursor dir cursor
      selected' = fmap (moveTo cursor) selected
  in w { cursor = nc, selected = selected' }

update Interact w@World{cursor, sprites, selected} =
  let selected' = find (isOnTopOf cursor) sprites
  in maybe w { selected = selected', sprites = maybe sprites (:sprites) selected}
           (\sel -> w { selected = Just sel, sprites = sprites \\ [sel] })
           selected'



moveCursor :: Direction -> Position -> Position
moveCursor dir (x,y) = case dir of
  North -> (x, y-1)
  South -> (x, y+1)
  East  -> (x+1, y)
  West  -> (x-1, y)


loop :: World -> IO ()
loop w = do
  putStrLn $ draw w
  cmd <- getLine
  let command = parseInput cmd
  system "cls"
  let nw = update command w  
  loop nw


parseInput :: String -> Command
parseInput s = case s of
  'w':[] -> Move North
  's':[] -> Move South
  'a':[] -> Move West
  'd':[] -> Move East
  _ -> Interact


main :: IO ()
main = do
  editor
  --loader



loader = do
  contents <- readFile "bait.txt"
  writeFile "output.txt" $ unlines $ map (\l -> "\"" ++ l ++ "\",") $ formatMap contents


formatMap :: String -> [String]
formatMap = tail . init . map (init . tail) . lines

house :: Sprite
house = readSprite "House" " _____ \n/     \\\n███████\n█▄█ █▄█\n"
