-- Shared helpers for the gloss versions of the maps.
--
-- The maps were first written for raylib, where the origin is the top-left
-- corner and y points down. All the map maths still works that way, so these
-- helpers take raylib-style "canvas" points and turn them into gloss
-- coordinates (origin in the centre, y pointing up) at the very last moment.
module MapArt
  ( Art (..),
    View,
    readRGBA8,
    loadArtSet,
    toGloss,
    blit,
    polygonAt,
    lineAt,
    rectAt,
    discAt,
    ringAt,
    skyBands,
    rgb,
    rgba,
  )
where

import Codec.Picture (Image, PixelRGBA8, convertRGBA8, imageHeight, imageWidth, readImage)
import qualified Data.Map.Strict as M
import Graphics.Gloss
import Graphics.Gloss.Juicy (fromImageRGBA8)

-- A picture together with its size, since a gloss Picture doesn't know its own size
data Art = Art
  { artPic :: Picture,
    artW :: Float,
    artH :: Float
  }

-- The size of the area the map is drawn into, in canvas pixels
type View = (Float, Float)

readRGBA8 :: FilePath -> IO (Image PixelRGBA8)
readRGBA8 path = do
  result <- readImage path
  case result of
    Left err -> ioError (userError ("could not load " ++ path ++ ": " ++ err))
    Right img -> pure (convertRGBA8 img)

loadArt :: FilePath -> IO Art
loadArt path = do
  img <- readRGBA8 path
  pure
    Art
      { artPic = fromImageRGBA8 img,
        artW = fromIntegral (imageWidth img),
        artH = fromIntegral (imageHeight img)
      }

-- Loads folder/name.png for every name
loadArtSet :: FilePath -> [String] -> IO (M.Map String Art)
loadArtSet folder names =
  M.fromList . zip names <$> mapM (\n -> loadArt (folder ++ "/" ++ n ++ ".png")) names

-- canvas point (top-left origin, y down) -> gloss point (centre origin, y up)
toGloss :: View -> (Float, Float) -> (Float, Float)
toGloss (vw, vh) (x, y) = (x - vw / 2, vh / 2 - y)

-- Draws art with its top-left corner at canvas point (x, y), like raylib's drawTexture
blit :: View -> Art -> Float -> Float -> Picture
blit view art x y =
  let (gx, gy) = toGloss view (fromIntegral (round x :: Int) + artW art / 2, fromIntegral (round y :: Int) + artH art / 2)
   in translate gx gy (artPic art)

polygonAt :: View -> Color -> [(Float, Float)] -> Picture
polygonAt view col ps = color col (polygon (map (toGloss view) ps))

lineAt :: View -> Color -> (Float, Float) -> (Float, Float) -> Picture
lineAt view col a b = color col (line [toGloss view a, toGloss view b])

-- Filled rectangle from its top-left corner, like raylib's drawRectangle
rectAt :: View -> Color -> Float -> Float -> Float -> Float -> Picture
rectAt view col x y w h = polygonAt view col [(x, y), (x + w, y), (x + w, y + h), (x, y + h)]

discAt :: View -> Color -> Float -> (Float, Float) -> Picture
discAt view col r p = let (gx, gy) = toGloss view p in translate gx gy (color col (circleSolid r))

ringAt :: View -> Color -> Float -> (Float, Float) -> Picture
ringAt view col r p = let (gx, gy) = toGloss view p in translate gx gy (color col (circle r))

-- A vertical gradient made of 12 bands, from the top colour to the bottom colour
skyBands :: View -> (Int, Int, Int, Int) -> (Int, Int, Int, Int) -> Picture
skyBands view@(vw, vh) (r0, g0, b0, a0) (r1, g1, b1, a1) =
  pictures
    [ rectAt view (makeColorI (mix r0 r1) (mix g0 g1) (mix b0 b1) (mix a0 a1)) 0 (fromIntegral k * band) vw band
      | k <- [0 .. 11 :: Int],
        let f = fromIntegral k / 11 :: Float
            mix a b = round (fromIntegral a + fromIntegral (b - a) * f)
    ]
  where
    band = fromIntegral (floor (vh / 12) :: Int) + 1

rgb :: Int -> Int -> Int -> Color
rgb r g b = makeColorI r g b 255

rgba :: Int -> Int -> Int -> Int -> Color
rgba = makeColorI
