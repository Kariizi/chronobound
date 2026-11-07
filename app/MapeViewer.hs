import Model
import View
import Controller
<<<<<<< map-version2
module Main (main) where

import Control.Monad (unless)
import Raylib.Core
import Raylib.Core.Textures
import Raylib.Types hiding (Ray (..))
import Raylib.Util (drawing, textureMode, withWindow)

import Victoria


-- everything is drawn on a small canvas then scaled up without smoothing which keeps the pixel art crisp at any window size.
canvasW, canvasH :: Int
canvasW = 640
canvasH = 360

main :: IO ()
main = do
  setConfigFlags [WindowResizable, VsyncHint]
  withWindow (canvasW * 2) (canvasH * 2) "Victorian District" 60 $ \_ -> do
    setWindowMinSize canvasW canvasH
    district <- loadVictoria "assets"
    canvas <- loadRenderTexture canvasW canvasH
    _ <- setTextureFilter (renderTexture'texture canvas) TextureFilterPoint

    let loop cam = do
          closing <- windowShouldClose
          unless closing $ do
            -- fit the canvas in the window, at a whole-number scale if possible
            sw <- getScreenWidth
            sh <- getScreenHeight
            let fit = min (fromIntegral sw / fromIntegral canvasW) (fromIntegral sh / fromIntegral canvasH) :: Float
                scale = if fit >= 1 then fromIntegral (floor fit :: Int) else fit
                offX = (fromIntegral sw - fromIntegral canvasW * scale) / 2
                offY = (fromIntegral sh - fromIntegral canvasH * scale) / 2

            -- move the camera: keys, or dragging with the mouse
            dt <- getFrameTime
            let held ks = or <$> mapM isKeyDown ks
            l <- held [KeyLeft, KeyA]
            r <- held [KeyRight, KeyD]
            u <- held [KeyUp, KeyW]
            dn <- held [KeyDown, KeyS]
            dragging <- isMouseButtonDown MouseButtonLeft
            Vector2 mdx mdy <- getMouseDelta
            let speed = 320 * dt
                b x = if x then 1 else 0
                keyMove = (speed * (b r - b l), speed * (b dn - b u))
                dragMove = if dragging then (negate mdx / scale, negate mdy / scale) else (0, 0)
                cam' = clampView district (fst cam + fst keyMove + fst dragMove, snd cam + snd keyMove + snd dragMove)

            textureMode canvas (drawVictoria district (canvasW, canvasH) cam')
            drawing $ do
              clearBackground (Color 10 10 16 255)
              drawTexturePro
                (renderTexture'texture canvas)
                (Rectangle 0 0 (fromIntegral canvasW) (negate (fromIntegral canvasH)))
                (Rectangle offX offY (fromIntegral canvasW * scale) (fromIntegral canvasH * scale))
                (Vector2 0 0)
                0
                (Color 255 255 255 255)
            loop cam'
    loop (startView district)
=======
import System.Random (randomRIO)

zoneBounds :: Zone -> (Float, Float, Float, Float)
zoneBounds TopLeft     = (-maxX, -lineMargin,  lineMargin,  maxY)
zoneBounds TopRight    = ( lineMargin,  maxX,  lineMargin,  maxY)
zoneBounds BottomLeft  = (-maxX, -lineMargin, -maxY, -lineMargin)
zoneBounds BottomRight = ( lineMargin,  maxX, -maxY, -lineMargin)

maxX, maxY :: Float
maxX = worldHalfWidth  - edgeMargin
maxY = worldHalfHeight - edgeMargin

randomMobPositionsIn :: Zone -> Int -> [(Float, Float)] -> IO [(Float, Float)]
randomMobPositionsIn _ 0 positions = return positions

randomMobPositionsIn zone n positions = do
    let (x0, x1, y0, y1) = zoneBounds zone
    x <- randomRIO (x0, x1)
    y <- randomRIO (y0, y1)

    let newPosition = (x, y)

    if any (tooClose newPosition) (position initialPlayer : positions)
        then randomMobPositionsIn zone n positions
        else randomMobPositionsIn zone (n - 1) (newPosition : positions)



tooClose :: (Float, Float) -> (Float, Float) -> Bool
tooClose (x1, y1) (x2, y2) =
    (x2 - x1)^2 + (y2 - y1)^2 < 80^2

spawnZoneMobs :: Int -> Zone -> IO [Mob]
spawnZoneMobs n zone = do
    positions <- randomMobPositionsIn zone n []
    return (map (\pos -> initialMob { mobPosition = pos, mobZone = zone }) positions)

main :: IO ()
main = do
    startingMobs <- concat <$> mapM (spawnZoneMobs 2) [minBound .. maxBound]

    let startingWorld = initialWorld { mobs = startingMobs }

    play
        (InWindow "Chronobound" (round windowWidth, round windowHeight) (100, 100))
        white
        60
        startingWorld
        drawWorld
        handleInput
        updateWorld
>>>>>>> main
