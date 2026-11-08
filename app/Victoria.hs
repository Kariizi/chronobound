
module Victoria
  (
    Ground (..),
    HouseStyle (..),
    Thing (..),
    Tile (..),

    Victoria,
    loadVictoria,
    tileAt,

    drawVictoria,
    startView,
    clampView,
    houseCovers,
 )

where

import Codec.Picture (PixelRGBA8 (..), imageHeight, imageWidth, pixelAt)
import Data.Array (Array, bounds, listArray, range, (!))
import Data.Bits (shiftR, xor)
import Data.List (minimumBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Graphics.Gloss (Picture, pictures)
import MapArt


-- All the tile options
data Ground = Cobble | Pavement | Plaza | Grass | Water
  deriving (Eq, Show)

data HouseStyle = RedBrick | Stucco | Soot
  deriving (Eq, Show)

-- All the ground objects
data Thing = House HouseStyle | Lamp | Tree | Portal
  deriving (Eq, Show)

data Tile
  = Void -- empty air beyond the edge of the district
  | Tile Ground (Maybe Thing)
  deriving (Eq, Show)

-- The map's colour key transparent pixels are empty air. Any other
-- colour is matched to the nearest one here, so slightly off colours from a paint program still work

legend :: [((Int, Int, Int), Tile)]
legend =
  [ ((110, 110, 120), Tile Cobble Nothing),
    ((175, 170, 160), Tile Pavement Nothing),
    ((210, 195, 165), Tile Plaza Nothing),
    ((80, 135, 65), Tile Grass Nothing),
    ((40, 70, 105), Tile Water Nothing),
    ((160, 70, 55), Tile Pavement (Just (House RedBrick))),
    ((225, 212, 185), Tile Pavement (Just (House Stucco))),
    ((105, 80, 72), Tile Pavement (Just (House Soot))),
    ((255, 215, 90), Tile Pavement (Just Lamp)),
    ((35, 95, 45), Tile Grass (Just Tree)),
    ((0, 240, 255), Tile Plaza (Just Portal))
  ]

colourToTile :: PixelRGBA8 -> Tile
colourToTile (PixelRGBA8 r g b a)
  | a < 128 = Void
  | otherwise = snd (minimumBy (comparing (distance . fst)) legend)
  where
    distance (lr, lg, lb) = sq (fromIntegral r - lr) + sq (fromIntegral g - lg) + sq (fromIntegral b - lb)
    sq x = x * x


-- Tiles indexed by (column, row) of the PNG.
type Grid = Array (Int, Int) Tile

-- A loaded street ready to draw
data Victoria = Victoria
  { vGrid :: Grid,
    vArt :: M.Map String Art,
    vOrder :: [(Int, Int)], -- every tile, back to front
    vStart :: (Float, Float), -- camera point centred on the portal
    vLand :: (Float, Float, Float, Float) -- smallest and largest column and row with ground
  }

-- the assets are loaded here
loadVictoria :: FilePath -> IO Victoria
loadVictoria folder = do
  grid <- loadGrid (folder ++ "/map.png")
  art <- loadArtSet folder artNames
  let (_, (maxI, maxJ)) = bounds grid
      tiles = range (bounds grid)
      portals = [t | t <- tiles, lookupTile grid t == Tile Plaza (Just Portal)]
      land = [t | t <- tiles, lookupTile grid t /= Void]
      (is, js) = (map (fromIntegral . fst) land, map (fromIntegral . snd) land)
  pure
    Victoria
      { vGrid = grid,
        vArt = art,
        vOrder = sortOn (\(i, j) -> i + j) tiles,
        vStart = tileTop (case portals of (p : _) -> p; [] -> (maxI `div` 2, maxJ `div` 2)),
        vLand = (minimum is, maximum is + 1, minimum js, maximum js + 1)
      }

loadGrid :: FilePath -> IO Grid
loadGrid path = do
  image <- readRGBA8 path
  let w = imageWidth image
      h = imageHeight image
      grid = listArray ((0, 0), (w - 1, h - 1)) [colourToTile (pixelAt image i j) | i <- [0 .. w - 1], j <- [0 .. h - 1]]
  pure (listArray (bounds grid) [settle grid t (grid ! t) | t <- range (bounds grid)])

-- This makes the lamp stand on the correct tile
settle :: Grid -> (Int, Int) -> Tile -> Tile
settle grid (i, j) tile = case tile of
  Tile _ (Just Lamp) ->
    let around = [g | n <- [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)], Tile g Nothing <- [lookupTile grid n]]
        commonest = case sortOn (\g -> negate (length (filter (== g) around))) around of
          (g : _) -> g
          [] -> Pavement
     in Tile commonest (Just Lamp)
  _ -> tile

lookupTile :: Grid -> (Int, Int) -> Tile
lookupTile grid t@(i, j) =
  let (_, (maxI, maxJ)) = bounds grid
   in if i < 0 || j < 0 || i > maxI || j > maxJ then Void else grid ! t

tileAt :: Victoria -> (Int, Int) -> Tile
tileAt v = lookupTile (vGrid v)

tileTop :: (Int, Int) -> (Float, Float)
tileTop (i, j) = (fromIntegral (i - j) * 16, fromIntegral (i + j) * 8)

-- camera points
startView :: Victoria -> (Float, Float)
startView = vStart

clampView :: Victoria -> (Float, Float) -> (Float, Float)
clampView v (x, y) =
  let (loI, hiI, loJ, hiJ) = vLand v
      i = (x / 16 + y / 8) / 2
      j = (y / 8 - x / 16) / 2
      ci = max loI (min hiI i)
      cj = max loJ (min hiJ j)
   in ((ci - cj) * 16, (ci + cj) * 8)


artNames :: [String]
artNames =
  [ "cobble", "pavement", "plaza", "grass", "water",
    "house_red", "house_red_chimney", "house_stucco", "house_stucco_chimney",
    "house_soot", "house_soot_chimney", "lamp", "tree", "portal"
  ]

groundArt :: Ground -> String
groundArt g = case g of
  Cobble -> "cobble"
  Pavement -> "pavement"
  Plaza -> "plaza"
  Grass -> "grass"
  Water -> "water"

-- which picture to use and how many rows of the picture sit below its footing so it stands in the right place on its tile
thingArt :: (Int, Int) -> Thing -> (String, Int)
thingArt t thing = case thing of
  House style ->
    let base = case style of
          RedBrick -> "house_red"
          Stucco -> "house_stucco"
          Soot -> "house_soot"
     in (if hashT t `mod` 10 < 3 then base ++ "_chimney" else base, 0)
  Lamp -> ("lamp", 1)
  Tree -> ("tree", 1)
  Portal -> ("portal", 10)

-- random number for a tile
hashT :: (Int, Int) -> Int
hashT (x, y) = let h = (x * 73856093) `xor` (y * 19349663) `xor` 0x5bd1e995 in abs (h `xor` (h `shiftR` 13))


-- Draw the street into an area of the given size in canvas pixels, with the camera point at its centre.
-- The result is a gloss Picture centred on (0, 0).
drawVictoria :: Victoria -> View -> (Float, Float) -> Picture
drawVictoria v view@(viewW, viewH) (camX, camY) =
  pictures ([sky] ++ concatMap ground onScreen ++ concatMap thing onScreen)
  where
    -- evening sky
    sky = skyBands view (22, 24, 38, 255) (64, 56, 66, 255)

    cx = fromIntegral (round camX :: Int)
    cy = fromIntegral (round camY :: Int)
    place t = let (x, y) = tileTop t in (x - cx + viewW / 2, y - cy + viewH / 2)
    visible (x, y) = x > -48 && x < viewW + 48 && y > -24 && y < viewH + 96
    onScreen = [(t, p) | t <- vOrder v, let p = place t, visible p]
    at = tileAt v
    art = vArt v

    -- the ground with stone where the streets end
    ground (t@(i, j), (x, y)) = case at t of
      Void -> []
      Tile g _ ->
        let right = (x + 16, y + 8)
            bottom = (x, y + 16)
            left = (x - 16, y + 8)
            down (px, py) k = (px, py + k)
            wall = 20
            rightWall =
              [polygonAt view (rgb 124 118 110) [right, bottom, down bottom wall, down right wall] | at (i + 1, j) == Void]
                ++ [lineAt view (rgb 96 92 86) (down right k) (down bottom k) | at (i + 1, j) == Void, k <- [6, 12, 18]]
            leftWall =
              [polygonAt view (rgb 92 88 84) [bottom, left, down left wall, down bottom wall] | at (i, j + 1) == Void]
                ++ [lineAt view (rgb 72 70 66) (down left k) (down bottom k) | at (i, j + 1) == Void, k <- [6, 12, 18]]
         in rightWall ++ leftWall ++ [blit view (art M.! groundArt g) (x - 16) y]

    -- houses, lamps, trees and the portal
    thing (t, (x, y)) = case at t of
      Tile _ (Just th) ->
        let (name, below) = thingArt t th
            a = art M.! name
         in case th of
              House _ -> [blit view a (x - 16) (y + 16 - artH a)] -- footprint fills the tile
              _ -> [blit view a (x - artW a / 2) (y + 8 + fromIntegral below - artH a)] -- stands on the tile's centre
      _ -> []


-- True if a map point lies on a house tile or under the drawn house picture
houseCovers :: Victoria -> (Float, Float) -> Bool
houseCovers v (mx, my) = any covers nearby
  where
    i = (mx / 16 + my / 8) / 2
    j = (my / 8 - mx / 16) / 2
    -- house pictures stretch upwards, so the house is at or below the point
    nearby = [ (a, b) | a <- [floor i - 2 .. floor i + 8]
                      , b <- [floor j - 2 .. floor j + 8] ]
    covers t = case tileAt v t of
      Tile _ (Just (House style)) ->
        let -- use the plain house (no chimney) so chimneys don't block
            name = case style of
              RedBrick -> "house_red"
              Stucco   -> "house_stucco"
              Soot     -> "house_soot"
            a        = vArt v M.! name
            (hx, hy) = tileTop t
            dx       = abs (mx - hx)
            -- diamond-shaped roof on top, diamond-shaped footprint at the bottom
            top      = hy + 16 - artH a + dx / 2
            bottom   = hy + 16 - dx / 2
         in dx <= 16 && my >= top && my <= bottom
      _ -> False