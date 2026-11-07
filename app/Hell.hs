
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

import Codec.Picture (PixelRGBA8 (..), imageHeight, imageWidth, pixelAt)
import Data.Array (Array, bounds, listArray, range, (!))
import Data.Bits (shiftR, xor)
import Data.List (minimumBy, sortOn)
import qualified Data.Map.Strict as M
import Data.Ord (comparing)
import Graphics.Gloss (Picture, pictures)
import MapArt


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

colourToTile :: PixelRGBA8 -> Tile
colourToTile (PixelRGBA8 r g b a)
  | a < 128 = Void
  | otherwise = snd (minimumBy (comparing (distance . fst)) legend)
  where
    distance (lr, lg, lb) = sq (fromIntegral r - lr) + sq (fromIntegral g - lg) + sq (fromIntegral b - lb)
    sq x = x * x



type Grid = Array (Int, Int) Tile


data Hell = Hell
  { hGrid :: Grid,
    hArt :: M.Map String Art,
    hOrder :: [(Int, Int)],
    hStart :: (Float, Float),
    hLand :: (Float, Float, Float, Float)
  }


loadHell :: FilePath -> IO Hell
loadHell folder = do
  grid <- loadGrid (folder ++ "/map.png")
  art <- loadArtSet folder artNames
  let (_, (maxI, maxJ)) = bounds grid
      tiles = range (bounds grid)
      portals = [t | t <- tiles, lookupTile grid t == Tile Brimstone (Just Portal)]
      land = [t | t <- tiles, lookupTile grid t /= Void]
      (is, js) = (map (fromIntegral . fst) land, map (fromIntegral . snd) land)
  pure
    Hell
      { hGrid = grid,
        hArt = art,
        hOrder = sortOn (\(i, j) -> i + j) tiles,
        hStart = tileTop (case portals of (p : _) -> p; [] -> (maxI `div` 2, maxJ `div` 2)),
        hLand = (minimum is, maximum is + 1, minimum js, maximum js + 1)
      }

loadGrid :: FilePath -> IO Grid
loadGrid path = do
  image <- readRGBA8 path
  let w = imageWidth image
      h = imageHeight image
      grid = listArray ((0, 0), (w - 1, h - 1)) [colourToTile (pixelAt image i j) | i <- [0 .. w - 1], j <- [0 .. h - 1]]
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


-- Draw hell into an area of the given size in canvas pixels, with the camera point at its centre.
-- The result is a gloss Picture centred on (0, 0).
drawHell :: Hell -> View -> (Float, Float) -> Picture
drawHell h view@(viewW, viewH) (camX, camY) =
  pictures ([sky] ++ embers ++ concatMap ground onScreen ++ concatMap thing onScreen)
  where
    sky = skyBands view (18, 8, 10, 255) (120, 30, 20, 255)

    embers =
      [ rectAt view (rgb 255 140 60) (fromIntegral (floor (x * viewW / 640) :: Int)) (fromIntegral (floor (y * viewH / 360) :: Int)) 1 1
        | (x, y) <- [(41, 30), (97, 72), (152, 18), (228, 51), (309, 36), (371, 80), (446, 24), (519, 63), (583, 41), (612, 92), (66, 118), (548, 130)]
      ]

    cx = fromIntegral (round camX :: Int)
    cy = fromIntegral (round camY :: Int)
    place t = let (x, y) = tileTop t in (x - cx + viewW / 2, y - cy + viewH / 2)
    visible (x, y) = x > -48 && x < viewW + 48 && y > -24 && y < viewH + 96
    onScreen = [(t, p) | t <- hOrder h, let p = place t, visible p]
    at = tileAt h
    art = hArt h

    ground (t@(i, j), (x, y)) = case at t of
      Void -> []
      Tile g _ ->
        let right = (x + 16, y + 8)
            bottom = (x, y + 16)
            left = (x - 16, y + 8)
            down (px, py) k = (px, py + k)
            wall = 20
            rightWall
              | at (i + 1, j) == Void =
                  [ polygonAt view (rgb 70 40 36) [right, bottom, down bottom wall, down right wall],
                    lineAt view (rgb 44 24 22) (down right 7) (down bottom 7),
                    lineAt view (rgb 220 90 30) (down right 13) (down bottom 13),
                    lineAt view (rgb 44 24 22) (down right 18) (down bottom 18)
                  ]
              | otherwise = []
            leftWall
              | at (i, j + 1) == Void =
                  [ polygonAt view (rgb 50 28 26) [bottom, left, down left wall, down bottom wall],
                    lineAt view (rgb 32 18 16) (down left 7) (down bottom 7),
                    lineAt view (rgb 190 70 24) (down left 13) (down bottom 13),
                    lineAt view (rgb 32 18 16) (down left 18) (down bottom 18)
                  ]
              | otherwise = []
         in rightWall ++ leftWall ++ [blit view (art M.! groundArt g) (x - 16) y]

    thing (t, (x, y)) = case at t of
      Tile _ (Just th) ->
        let (name, below) = thingArt t th
            a = art M.! name
         in case th of
              Building _ -> [blit view a (x - 16) (y + 16 - artH a)]
              _ -> [blit view a (x - artW a / 2) (y + 8 + fromIntegral below - artH a)]
      _ -> []
