
module Limbo
  (
    Ground (..),
    HouseStyle (..),
    Thing (..),
    Tile (..),


    Limbo,
    loadLimbo,
    tileAt,


    drawLimbo,
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
import Raylib.Core (beginBlendMode, endBlendMode)
import Raylib.Core.Shapes
import Raylib.Core.Textures
import Raylib.Types hiding (Ray (..))


data Ground
  = Cobble
  | Flagstone
  | Slate
  | Grass 
  | Ink
  deriving (Eq, Show)

data HouseStyle = Ashen | Pale | Black
  deriving (Eq, Show)


data Thing
  = House HouseStyle
  | Lamp
  | Tree
  | Portal
  | ClockTower
  | TowerBase
  deriving (Eq, Show)

data Tile
  = Void
  | Tile Ground (Maybe Thing)
  deriving (Eq, Show)

legend :: [((Int, Int, Int), Tile)]
legend =
  [ ((90, 90, 100), Tile Cobble Nothing),
    ((150, 150, 160), Tile Flagstone Nothing),
    ((120, 115, 140), Tile Slate Nothing),
    ((110, 115, 85), Tile Grass Nothing),
    ((20, 20, 35), Tile Ink Nothing),
    ((100, 100, 105), Tile Flagstone (Just (House Ashen))),
    ((200, 210, 200), Tile Flagstone (Just (House Pale))),
    ((45, 40, 50), Tile Flagstone (Just (House Black))),
    ((120, 255, 200), Tile Flagstone (Just Lamp)),
    ((70, 65, 60), Tile Grass (Just Tree)),
    ((180, 120, 255), Tile Slate (Just Portal)),
    ((250, 250, 250), Tile Slate (Just ClockTower))
  ]

colourToTile :: Color -> Tile
colourToTile (Color r g b a)
  | a < 128 = Void
  | otherwise = snd (minimumBy (comparing (distance . fst)) legend)
  where
    distance (lr, lg, lb) = sq (fromIntegral r - lr) + sq (fromIntegral g - lg) + sq (fromIntegral b - lb)
    sq x = x * x


type Grid = Array (Int, Int) Tile

data Limbo = Limbo
  { lGrid :: Grid,
    lArt :: M.Map String Texture,
    lOrder :: [(Int, Int)],
    lStart :: (Float, Float),
    lLand :: (Float, Float, Float, Float)

loadLimbo :: FilePath -> IO Limbo
loadLimbo folder = do
  grid <- loadGrid (folder ++ "/map.png")
  textures <- mapM (\n -> loadTexture (folder ++ "/" ++ n ++ ".png")) artNames
  let (_, (maxI, maxJ)) = bounds grid
      tiles = range (bounds grid)
      find thing = [t | t <- tiles, lookupTile grid t == Tile Slate (Just thing)]
      land = [t | t <- tiles, lookupTile grid t /= Void]
      (is, js) = (map (fromIntegral . fst) land, map (fromIntegral . snd) land)
      start = case (find ClockTower, find Portal) of
        
        (t : _, _) -> let (x, y) = tileTop t in (x, y + 8 - 106)
        ([], p : _) -> tileTop p
        ([], []) -> tileTop (maxI `div` 2, maxJ `div` 2)
  pure
    Limbo
      { lGrid = grid,
        lArt = M.fromList (zip artNames textures),
        lOrder = sortOn (\(i, j) -> i + j) tiles,
        lStart = start,
        lLand = (minimum is, maximum is + 1, minimum js, maximum js + 1)
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
  Tile _ (Just Lamp) ->
    let around = [g | n <- [(i + 1, j), (i - 1, j), (i, j + 1), (i, j - 1)], Tile g Nothing <- [lookupTile grid n]]
        commonest = case sortOn (\g -> negate (length (filter (== g) around))) around of
          (g : _) -> g
          [] -> Flagstone
     in Tile commonest (Just Lamp)
  Tile g Nothing
    | or [lookupTile grid (i + di, j + dj) == Tile Slate (Just ClockTower) | di <- [-1, 0, 1], dj <- [-1, 0, 1]] ->
        Tile g (Just TowerBase)
  _ -> tile

lookupTile :: Grid -> (Int, Int) -> Tile
lookupTile grid t@(i, j) =
  let (_, (maxI, maxJ)) = bounds grid
   in if i < 0 || j < 0 || i > maxI || j > maxJ then Void else grid ! t


tileAt :: Limbo -> (Int, Int) -> Tile
tileAt l = lookupTile (lGrid l)


tileTop :: (Int, Int) -> (Float, Float)
tileTop (i, j) = (fromIntegral (i - j) * 16, fromIntegral (i + j) * 8)


startView :: Limbo -> (Float, Float)
startView = lStart


clampView :: Limbo -> (Float, Float) -> (Float, Float)
clampView l (x, y) =
  let (loI, hiI, loJ, hiJ) = lLand l
      i = (x / 16 + y / 8) / 2
      j = (y / 8 - x / 16) / 2
      ci = max loI (min hiI i)
      cj = max loJ (min hiJ j)
   in ((ci - cj) * 16, (ci + cj) * 8)


artNames :: [String]
artNames =
  [ "cobble", "flagstone", "slate", "grass", "ink",
    "house_ashen", "house_ashen_chimney", "house_pale", "house_pale_chimney",
    "house_black", "house_black_chimney", "lamp", "tree", "portal", "clocktower"
  ]

groundArt :: Ground -> String
groundArt g = case g of
  Cobble -> "cobble"
  Flagstone -> "flagstone"
  Slate -> "slate"
  Grass -> "grass"
  Ink -> "ink"


thingArt :: (Int, Int) -> Thing -> Maybe (String, Int)
thingArt t thing = case thing of
  House style ->
    let base = case style of
          Ashen -> "house_ashen"
          Pale -> "house_pale"
          Black -> "house_black"
     in Just (if hashT t `mod` 10 < 3 then base ++ "_chimney" else base, 0)
  Lamp -> Just ("lamp", 1)
  Tree -> Just ("tree", 1)
  Portal -> Just ("portal", 10)
  ClockTower -> Just ("clocktower", 24)
  TowerBase -> Nothing


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


ghostClock :: (Int, Int) -> (Float, Float, Float, Float) -> IO ()
ghostClock (viewW, viewH) (fx, fy, r, minutes) = do
  let cx = fx * fromIntegral viewW
      cy = fy * fromIntegral viewH
      faint = Color 214 208 240 46
      hand len turns =
        let a = turns * 2 * pi
         in drawLineV (Vector2 cx cy) (Vector2 (cx + len * sin a) (cy - len * cos a)) faint
  drawCircleLinesV (Vector2 cx cy) r faint
  drawCircleLinesV (Vector2 cx cy) (r - 2) (Color 214 208 240 22)
  forM_ [0 .. 11 :: Int] $ \k -> do
    let a = fromIntegral k / 12 * 2 * pi
    drawPixelV (Vector2 (cx + (r - 4) * sin a) (cy - (r - 4) * cos a)) faint
  hand (r * 0.8) (minutes / 60)
  hand (r * 0.5) ((11 + minutes / 60) / 12)


drawLimbo :: Limbo -> (Int, Int) -> (Float, Float) -> IO ()
drawLimbo l (viewW, viewH) (camX, camY) = do
  let vw = fromIntegral viewW :: Float
      vh = fromIntegral viewH :: Float

  forM_ [0 .. 11 :: Int] $ \k -> do
    let f = fromIntegral k / 11 :: Float
        mix a b = round (a + (b - a) * f) :: Word8
        band = viewH `div` 12 + 1
    drawRectangle 0 (k * band) viewW band (rgb (mix 24 92) (mix 20 86) (mix 40 116))

  let moon = Vector2 (vw * 0.82) (vh * 0.17)
  drawCircleV moon 30 (Color 220 230 210 22)
  drawCircleV moon 24 (Color 220 230 210 30)
  drawCircleV moon 18 (rgb 224 230 212)
  drawCircleV (Vector2 (vw * 0.82 - 6) (vh * 0.17 - 4)) 4 (rgb 200 206 190)
  drawCircleV (Vector2 (vw * 0.82 + 5) (vh * 0.17 + 6)) 3 (rgb 204 210 194)

  mapM_
    (ghostClock (viewW, viewH))
    [(0.12, 0.14, 20, 57), (0.38, 0.08, 13, 52), (0.6, 0.22, 17, 59), (0.27, 0.3, 9, 48), (0.93, 0.36, 11, 55)]

  let cx = fromIntegral (round camX :: Int)
      cy = fromIntegral (round camY :: Int)
      place t = let (x, y) = tileTop t in (x - cx + vw / 2, y - cy + vh / 2)
      placed = [(t, place t) | t <- lOrder l]

      groundVisible (x, y) = x > -48 && x < vw + 48 && y > -24 && y < vh + 96
      thingVisible (x, y) = x > -64 && x < vw + 64 && y > -40 && y < vh + 260
      at = tileAt l
      art = lArt l


  forM_ [p | p@(_, xy) <- placed, groundVisible xy] $ \(t@(i, j), (x, y)) -> case at t of
    Void -> pure ()
    Tile g _ -> do
      let right = (x + 16, y + 8)
          bottom = (x, y + 16)
          left = (x - 16, y + 8)
          down (px, py) k = (px, py + k)
          wall = 20
      when (at (i + 1, j) == Void) $ do
        quad right bottom (down bottom wall) (down right wall) (rgb 78 74 92)
        forM_ [6, 12, 18] $ \k -> drawLineV (vec (down right k)) (vec (down bottom k)) (rgb 56 52 68)
      when (at (i, j + 1) == Void) $ do
        quad bottom left (down left wall) (down bottom wall) (rgb 58 54 70)
        forM_ [6, 12, 18] $ \k -> drawLineV (vec (down left k)) (vec (down bottom k)) (rgb 42 40 52)
      blit (art M.! groundArt g) (x - 16) y


  forM_ [p | p@(_, xy) <- placed, thingVisible xy] $ \(t, (x, y)) -> case at t of
    Tile _ (Just thing) | Just (name, below) <- thingArt t thing -> do
      let tex = art M.! name
          w = fromIntegral (texture'width tex)
          ht = fromIntegral (texture'height tex)
      case thing of
        House _ -> blit tex (x - 16) (y + 16 - ht) 
        _ -> blit tex (x - w / 2) (y + 8 + fromIntegral below - ht)
    _ -> pure ()


  drawRectangleGradientV 0 0 viewW viewH (Color 170 160 200 64) (Color 170 160 200 18)

  beginBlendMode BlendAdditive
  drawRectangle 0 0 viewW viewH (Color 0 0 0 255)
  endBlendMode
