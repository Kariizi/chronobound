
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
 )

where

import Control.Monad (forM_, when)
import Data.Array (Array, bounds, listArray, range, (!))
import Data.Bits (shiftR, xor)
import Data.List (minimumBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Data.Word (Word8)
import Raylib.Core.Shapes
import Raylib.Core.Textures
import Raylib.Types hiding (Ray (..))


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

colourToTile :: Color -> Tile
colourToTile (Color r g b a)
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
    vArt :: M.Map String Texture,
    vOrder :: [(Int, Int)], -- every tile, back to front
    vStart :: (Float, Float), -- camera point centred on the portal
    vLand :: (Float, Float, Float, Float) -- smallest and largest column and row with ground
  }

-- the assets are loaded here
loadVictoria :: FilePath -> IO Victoria
loadVictoria folder = do
  grid <- loadGrid (folder ++ "/map.png")
  textures <- mapM (\n -> loadTexture (folder ++ "/" ++ n ++ ".png")) artNames
  let (_, (maxI, maxJ)) = bounds grid
      tiles = range (bounds grid)
      portals = [t | t <- tiles, lookupTile grid t == Tile Plaza (Just Portal)]
      land = [t | t <- tiles, lookupTile grid t /= Void]
      (is, js) = (map (fromIntegral . fst) land, map (fromIntegral . snd) land)
  pure
    Victoria
      { vGrid = grid,
        vArt = M.fromList (zip artNames textures),
        vOrder = sortOn (\(i, j) -> i + j) tiles,
        vStart = tileTop (case portals of (p : _) -> p; [] -> (maxI `div` 2, maxJ `div` 2)),
        vLand = (minimum is, maximum is + 1, minimum js, maximum js + 1)
      }

loadGrid :: FilePath -> IO Grid
loadGrid path = do
  image <- loadImage path
  colours <- loadImageColors image -- row by row and top to bottom
  let w = image'width image
      h = image'height image
      rows = listArray ((0, 0), (h - 1, w - 1)) (map colourToTile colours)
      grid = listArray ((0, 0), (w - 1, h - 1)) [rows ! (j, i) | i <- [0 .. w - 1], j <- [0 .. h - 1]]
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

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = Color r g b 255

blit :: Texture -> Float -> Float -> IO ()
blit tex x y = drawTexture tex (round x) (round y) (rgb 255 255 255)

-- filled triangle
tri :: (Float, Float) -> (Float, Float) -> (Float, Float) -> Color -> IO ()
tri a b c col
  | cross < 0 = drawTriangle (vec a) (vec b) (vec c) col
  | otherwise = drawTriangle (vec a) (vec c) (vec b) col
  where
    cross = (fst b - fst a) * (snd c - snd a) - (snd b - snd a) * (fst c - fst a)

quad :: (Float, Float) -> (Float, Float) -> (Float, Float) -> (Float, Float) -> Color -> IO ()
quad a b c d col = tri a b c col >> tri a c d col

vec :: (Float, Float) -> Vector2
vec (x, y) = Vector2 x y

--  Draw the street on the current target of the given size in pixels, with the camera point at its centre
drawVictoria :: Victoria -> (Int, Int) -> (Float, Float) -> IO ()
drawVictoria v (viewW, viewH) (camX, camY) = do
  -- evening sky
  forM_ [0 .. 11 :: Int] $ \k -> do
    let f = fromIntegral k / 11 :: Float
        mix a b = round (a + (b - a) * f) :: Word8
        band = viewH `div` 12 + 1
    drawRectangle 0 (k * band) viewW band (rgb (mix 22 64) (mix 24 56) (mix 38 66))

  let cx = fromIntegral (round camX :: Int)
      cy = fromIntegral (round camY :: Int)
      place t = let (x, y) = tileTop t in (x - cx + fromIntegral viewW / 2, y - cy + fromIntegral viewH / 2)
      visible (x, y) = x > -48 && x < fromIntegral viewW + 48 && y > -24 && y < fromIntegral viewH + 96
      onScreen = [(t, p) | t <- vOrder v, let p = place t, visible p]
      at = tileAt v
      art = vArt v

  -- the ground with stone where the streets end
  forM_ onScreen $ \(t@(i, j), (x, y)) -> case at t of
    Void -> pure ()
    Tile g _ -> do
      let right = (x + 16, y + 8)
          bottom = (x, y + 16)
          left = (x - 16, y + 8)
          down (px, py) k = (px, py + k)
          wall = 20
      when (at (i + 1, j) == Void) $ do
        quad right bottom (down bottom wall) (down right wall) (rgb 124 118 110)
        forM_ [6, 12, 18] $ \k -> drawLineV (vec (down right k)) (vec (down bottom k)) (rgb 96 92 86)
      when (at (i, j + 1) == Void) $ do
        quad bottom left (down left wall) (down bottom wall) (rgb 92 88 84)
        forM_ [6, 12, 18] $ \k -> drawLineV (vec (down left k)) (vec (down bottom k)) (rgb 72 70 66)
      blit (art M.! groundArt g) (x - 16) y

  -- this is houses, lamps, trees and the portal
  forM_ onScreen $ \(t, (x, y)) -> case at t of
    Tile _ (Just thing) -> do
      let (name, below) = thingArt t thing
          tex = art M.! name
          w = fromIntegral (texture'width tex)
          h = fromIntegral (texture'height tex)
      case thing of
        House _ -> blit tex (x - 16) (y + 16 - h) -- footprint fills the tile
        _ -> blit tex (x - w / 2) (y + 8 + fromIntegral below - h) -- stands on the tile's centre
    _ -> pure ()