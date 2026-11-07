
module Hell
  (
    Ground (..),
    BuildingStyle (..),
    Thing (..),
    Tile (..),

    Hell,
    loadHell,
    tileAt,


    drawHell,
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


data Ground
  = Basalt
  | Scorched
  | Brimstone
  | Ash
  | Lava
  deriving (Eq, Show)

data BuildingStyle = Charred | Bone | Obsidian
  deriving (Eq, Show)


data Thing = Building BuildingStyle | Brazier | DeadTree | Portal
  deriving (Eq, Show)

data Tile
  = Void
  | Tile Ground (Maybe Thing)
  deriving (Eq, Show)


legend :: [((Int, Int, Int), Tile)]
legend =
  [ ((60, 50, 55), Tile Basalt Nothing),
    ((120, 70, 55), Tile Scorched Nothing),
    ((200, 170, 70), Tile Brimstone Nothing),
    ((140, 135, 130), Tile Ash Nothing),
    ((255, 110, 20), Tile Lava Nothing),
    ((40, 30, 30), Tile Scorched (Just (Building Charred))),
    ((230, 220, 190), Tile Scorched (Just (Building Bone))),
    ((70, 40, 90), Tile Scorched (Just (Building Obsidian))),
    ((255, 60, 40), Tile Scorched (Just Brazier)),
    ((90, 60, 40), Tile Ash (Just DeadTree)),
    ((255, 0, 120), Tile Brimstone (Just Portal))
  ]

colourToTile :: Color -> Tile
colourToTile (Color r g b a)
  | a < 128 = Void
  | otherwise = snd (minimumBy (comparing (distance . fst)) legend)
  where
    distance (lr, lg, lb) = sq (fromIntegral r - lr) + sq (fromIntegral g - lg) + sq (fromIntegral b - lb)
    sq x = x * x



type Grid = Array (Int, Int) Tile


data Hell = Hell
  { hGrid :: Grid,
    hArt :: M.Map String Texture,
    hOrder :: [(Int, Int)],
    hStart :: (Float, Float),
    hLand :: (Float, Float, Float, Float)
  }


loadHell :: FilePath -> IO Hell
loadHell folder = do
  grid <- loadGrid (folder ++ "/map.png")
  textures <- mapM (\n -> loadTexture (folder ++ "/" ++ n ++ ".png")) artNames
  let (_, (maxI, maxJ)) = bounds grid
      tiles = range (bounds grid)
      portals = [t | t <- tiles, lookupTile grid t == Tile Brimstone (Just Portal)]
      land = [t | t <- tiles, lookupTile grid t /= Void]
      (is, js) = (map (fromIntegral . fst) land, map (fromIntegral . snd) land)
  pure
    Hell
      { hGrid = grid,
        hArt = M.fromList (zip artNames textures),
        hOrder = sortOn (\(i, j) -> i + j) tiles,
        hStart = tileTop (case portals of (p : _) -> p; [] -> (maxI `div` 2, maxJ `div` 2)),
        hLand = (minimum is, maximum is + 1, minimum js, maximum js + 1)
      }

loadGrid :: FilePath -> IO Grid
loadGrid path = do
  image <- loadImage path
  colours <- loadImageColors image
  let w = image'width image
      h = image'height image
      rows = listArray ((0, 0), (h - 1, w - 1)) (map colourToTile colours)
      grid = listArray ((0, 0), (w - 1, h - 1)) [rows ! (j, i) | i <- [0 .. w - 1], j <- [0 .. h - 1]]
  pure (listArray (bounds grid) [settle grid t (grid ! t) | t <- range (bounds grid)])


settle :: Grid -> (Int, Int) -> Tile -> Tile
settle grid (i, j) tile = case tile of
  Tile _ (Just Brazier) ->
    let around = [g | n <- [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)], Tile g Nothing <- [lookupTile grid n]]
        commonest = case sortOn (\g -> negate (length (filter (== g) around))) around of
          (g : _) -> g
          [] -> Scorched
     in Tile commonest (Just Brazier)
  _ -> tile

lookupTile :: Grid -> (Int, Int) -> Tile
lookupTile grid t@(i, j) =
  let (_, (maxI, maxJ)) = bounds grid
   in if i < 0 || j < 0 || i > maxI || j > maxJ then Void else grid ! t


tileAt :: Hell -> (Int, Int) -> Tile
tileAt h = lookupTile (hGrid h)


tileTop :: (Int, Int) -> (Float, Float)
tileTop (i, j) = (fromIntegral (i - j) * 16, fromIntegral (i + j) * 8)


startView :: Hell -> (Float, Float)
startView = hStart


clampView :: Hell -> (Float, Float) -> (Float, Float)
clampView h (x, y) =
  let (loI, hiI, loJ, hiJ) = hLand h
      i = (x / 16 + y / 8) / 2
      j = (y / 8 - x / 16) / 2
      ci = max loI (min hiI i)
      cj = max loJ (min hiJ j)
   in ((ci - cj) * 16, (ci + cj) * 8)


artNames :: [String]
artNames =
  [ "basalt", "scorched", "brimstone", "ash", "lava",
    "house_charred", "house_charred_spire", "house_bone", "house_bone_spire",
    "house_obsidian", "house_obsidian_spire", "brazier", "deadtree", "portal"
  ]

groundArt :: Ground -> String
groundArt g = case g of
  Basalt -> "basalt"
  Scorched -> "scorched"
  Brimstone -> "brimstone"
  Ash -> "ash"
  Lava -> "lava"

thingArt :: (Int, Int) -> Thing -> (String, Int)
thingArt t thing = case thing of
  Building style ->
    let base = case style of
          Charred -> "house_charred"
          Bone -> "house_bone"
          Obsidian -> "house_obsidian"
     in (if hashT t `mod` 10 < 3 then base ++ "_spire" else base, 0)
  Brazier -> ("brazier", 1)
  DeadTree -> ("deadtree", 1)
  Portal -> ("portal", 10)


hashT :: (Int, Int) -> Int
hashT (x, y) = let h = (x * 73856093) `xor` (y * 19349663) `xor` 0x5bd1e995 in abs (h `xor` (h `shiftR` 13))

rgb :: Word8 -> Word8 -> Word8 -> Color
rgb r g b = Color r g b 255

blit :: Texture -> Float -> Float -> IO ()
blit tex x y = drawTexture tex (round x) (round y) (rgb 255 255 255)


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


drawHell :: Hell -> (Int, Int) -> (Float, Float) -> IO ()
drawHell h (viewW, viewH) (camX, camY) = do

  forM_ [0 .. 11 :: Int] $ \k -> do
    let f = fromIntegral k / 11 :: Float
        mix a b = round (a + (b - a) * f) :: Word8
        band = viewH `div` 12 + 1
    drawRectangle 0 (k * band) viewW band (rgb (mix 18 120) (mix 8 30) (mix 10 20))

  forM_ [(41, 30), (97, 72), (152, 18), (228, 51), (309, 36), (371, 80), (446, 24), (519, 63), (583, 41), (612, 92), (66, 118), (548, 130)] $ \(x, y) ->
    drawRectangle (x * viewW `div` 640) (y * viewH `div` 360) 1 1 (rgb 255 140 60)

  let cx = fromIntegral (round camX :: Int)
      cy = fromIntegral (round camY :: Int)
      place t = let (x, y) = tileTop t in (x - cx + fromIntegral viewW / 2, y - cy + fromIntegral viewH / 2)
      visible (x, y) = x > -48 && x < fromIntegral viewW + 48 && y > -24 && y < fromIntegral viewH + 96
      onScreen = [(t, p) | t <- hOrder h, let p = place t, visible p]
      at = tileAt h
      art = hArt h


  forM_ onScreen $ \(t@(i, j), (x, y)) -> case at t of
    Void -> pure ()
    Tile g _ -> do
      let right = (x + 16, y + 8)
          bottom = (x, y + 16)
          left = (x - 16, y + 8)
          down (px, py) k = (px, py + k)
          wall = 20
      when (at (i + 1, j) == Void) $ do
        quad right bottom (down bottom wall) (down right wall) (rgb 70 40 36)
        drawLineV (vec (down right 7)) (vec (down bottom 7)) (rgb 44 24 22)
        drawLineV (vec (down right 13)) (vec (down bottom 13)) (rgb 220 90 30)
        drawLineV (vec (down right 18)) (vec (down bottom 18)) (rgb 44 24 22)
      when (at (i, j + 1) == Void) $ do
        quad bottom left (down left wall) (down bottom wall) (rgb 50 28 26)
        drawLineV (vec (down left 7)) (vec (down bottom 7)) (rgb 32 18 16)
        drawLineV (vec (down left 13)) (vec (down bottom 13)) (rgb 190 70 24)
        drawLineV (vec (down left 18)) (vec (down bottom 18)) (rgb 32 18 16)
      blit (art M.! groundArt g) (x - 16) y

  forM_ onScreen $ \(t, (x, y)) -> case at t of
    Tile _ (Just thing) -> do
      let (name, below) = thingArt t thing
          tex = art M.! name
          w = fromIntegral (texture'width tex)
          ht = fromIntegral (texture'height tex)
      case thing of
        Building _ -> blit tex (x - 16) (y + 16 - ht)
        _ -> blit tex (x - w / 2) (y + 8 + fromIntegral below - ht)
    _ -> pure ()
