
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
    limboBlocked,
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

colourToTile :: PixelRGBA8 -> Tile
colourToTile (PixelRGBA8 r g b a)
  | a < 128 = Void
  | otherwise = snd (minimumBy (comparing (distance . fst)) legend)
  where
    distance (lr, lg, lb) = sq (fromIntegral r - lr) + sq (fromIntegral g - lg) + sq (fromIntegral b - lb)
    sq x = x * x


type Grid = Array (Int, Int) Tile

data Limbo = Limbo
  { lGrid :: Grid,
    lArt :: M.Map String Art,
    lOrder :: [(Int, Int)],
    lStart :: (Float, Float),
    lLand :: (Float, Float, Float, Float),
    lTower :: Maybe (Int, Int)
  }

loadLimbo :: FilePath -> IO Limbo
loadLimbo folder = do
  grid <- loadGrid (folder ++ "/map.png")
  art <- loadArtSet folder artNames
  let (_, (maxI, maxJ)) = bounds grid
      tiles = range (bounds grid)
      find thing = [t | t <- tiles, lookupTile grid t == Tile Slate (Just thing)]
      land = [t | t <- tiles, lookupTile grid t /= Void]
      (is, js) = (map (fromIntegral . fst) land, map (fromIntegral . snd) land)
      start = case (find ClockTower, find Portal) of
        
        (t : _, _) -> let (x, y) = tileTop t in (x, y + 48 )
        ([], p : _) -> tileTop p
        ([], []) -> tileTop (maxI `div` 2, maxJ `div` 2)
  pure
    Limbo
      { lGrid = grid,
        lArt = art,
        lOrder = sortOn (\(i, j) -> i + j) tiles,
        lStart = start,
        lLand = (minimum is, maximum is + 1, minimum js, maximum js + 1),
        lTower = case find ClockTower of (t : _) -> Just t; [] -> Nothing
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


-- A faint clock face floating in the sky: (x and y as fractions of the view, radius, minutes past)
ghostClock :: View -> (Float, Float, Float, Float) -> Picture
ghostClock view@(viewW, viewH) (fx, fy, r, minutes) =
  pictures
    ( [ ringAt view faint r centre,
        ringAt view (rgba 214 208 240 22) (r - 2) centre
      ]
        ++ [rectAt view faint (cx + (r - 4) * sin a) (cy - (r - 4) * cos a) 1 1 | k <- [0 .. 11 :: Int], let a = fromIntegral k / 12 * 2 * pi]
        ++ [hand (r * 0.8) (minutes / 60), hand (r * 0.5) ((11 + minutes / 60) / 12)]
    )
  where
    cx = fx * viewW
    cy = fy * viewH
    centre = (cx, cy)
    faint = rgba 214 208 240 46
    hand len turns =
      let a = turns * 2 * pi
       in lineAt view faint centre (cx + len * sin a, cy - len * cos a)

-- Draw limbo into an area of the given size in canvas pixels, with the camera point at its centre.
-- The result is a gloss Picture centred on (0, 0).
drawLimbo :: Limbo -> View -> (Float, Float) -> Picture
drawLimbo l view@(vw, vh) (camX, camY) =
  pictures
    ( [sky]
        ++ moon
        ++ map (ghostClock view) [(0.12, 0.14, 20, 57), (0.38, 0.08, 13, 52), (0.6, 0.22, 17, 59), (0.27, 0.3, 9, 48), (0.93, 0.36, 11, 55)]
        ++ concatMap ground [p | p@(_, xy) <- placed, groundVisible xy]
        ++ concatMap thing [p | p@(_, xy) <- placed, thingVisible xy]
        ++ [haze]
    )
  where
    sky = skyBands view (24, 20, 40, 255) (92, 86, 116, 255)

    moon =
      [ discAt view (rgba 220 230 210 22) 30 (vw * 0.82, vh * 0.17),
        discAt view (rgba 220 230 210 30) 24 (vw * 0.82, vh * 0.17),
        discAt view (rgb 224 230 212) 18 (vw * 0.82, vh * 0.17),
        discAt view (rgb 200 206 190) 4 (vw * 0.82 - 6, vh * 0.17 - 4),
        discAt view (rgb 204 210 194) 3 (vw * 0.82 + 5, vh * 0.17 + 6)
      ]

    -- pale mist over everything, stronger at the top
    haze = skyBands view (170, 160, 200, 64) (170, 160, 200, 18)

    cx = fromIntegral (round camX :: Int)
    cy = fromIntegral (round camY :: Int)
    place t = let (x, y) = tileTop t in (x - cx + vw / 2, y - cy + vh / 2)
    placed = [(t, place t) | t <- lOrder l]

    groundVisible (x, y) = x > -48 && x < vw + 48 && y > -24 && y < vh + 96
    thingVisible (x, y) = x > -64 && x < vw + 64 && y > -40 && y < vh + 260
    at = tileAt l
    art = lArt l

    ground (t@(i, j), (x, y)) = case at t of
      Void -> []
      Tile g _ ->
        let right = (x + 16, y + 8)
            bottom = (x, y + 16)
            left = (x - 16, y + 8)
            down (px, py) k = (px, py + k)
            wall = 20
            rightWall =
              [polygonAt view (rgb 78 74 92) [right, bottom, down bottom wall, down right wall] | at (i + 1, j) == Void]
                ++ [lineAt view (rgb 56 52 68) (down right k) (down bottom k) | at (i + 1, j) == Void, k <- [6, 12, 18]]
            leftWall =
              [polygonAt view (rgb 58 54 70) [bottom, left, down left wall, down bottom wall] | at (i, j + 1) == Void]
                ++ [lineAt view (rgb 42 40 52) (down left k) (down bottom k) | at (i, j + 1) == Void, k <- [6, 12, 18]]
         in rightWall ++ leftWall ++ [blit view (art M.! groundArt g) (x - 16) y]

    thing (t, (x, y)) = case at t of
      Tile _ (Just th) | Just (name, below) <- thingArt t th ->
        let a = art M.! name
         in case th of
              House _ -> [blit view a (x - 16) (y + 16 - artH a)]
              _ -> [blit view a (x - artW a / 2) (y + 8 + fromIntegral below - artH a)]
      _ -> []

-- True if a map point can't be walked on: empty air, ink, houses or the clock tower
limboBlocked :: Limbo -> (Float, Float) -> Bool
limboBlocked l (mx, my) =
  case tileAt l (floor i, floor j) of
    Void                     -> True
    Tile Ink _               -> True
    Tile _ (Just ClockTower) -> True
    Tile _ (Just TowerBase)  -> True
    _                        -> any covers nearby || coversTower
  where
    i = (mx / 16 + my / 8) / 2
    j = (my / 8 - mx / 16) / 2
    nearby = [ (a, b) | a <- [floor i - 2 .. floor i + 8]
                      , b <- [floor j - 2 .. floor j + 8] ]

    -- houses: same shape as in Victoria, chimneys ignored
    covers t = case tileAt l t of
      Tile _ (Just (House style)) ->
        let name = case style of
              Ashen -> "house_ashen"
              Pale  -> "house_pale"
              Black -> "house_black"
            a        = lArt l M.! name
            (hx, hy) = tileTop t
            dx       = abs (mx - hx)
            top      = hy + 16 - artH a + dx / 2
            bottom   = hy + 16 - dx / 2
         in dx <= 16 && my >= top && my <= bottom
      _ -> False

    -- the tower stands on a 3x3 base, so it is 3 tiles wide
    coversTower = case lTower l of
      Nothing -> False
      Just t ->
        let a        = lArt l M.! "clocktower"
            (hx, hy) = tileTop t
            halfW    = min 48 (artW a / 2)
            dx       = abs (mx - hx)
            base     = hy + 32
            top      = base - artH a + dx / 2
            bottom   = base - dx / 2
         in dx <= halfW && my >= top && my <= bottom
