module Main where

import Graphics.Gloss.Interface.Pure.Game
import Model
import View
import Controller
import System.Random (randomRIO)


randomMobPositions :: Int -> [(Float, Float)] -> IO [(Float, Float)]
randomMobPositions 0 positions = return positions

randomMobPositions n positions = do
    x <- randomRIO (-370, 370)
    y <- randomRIO (-270, 270)

    let newPosition = (x, y)

    if any (tooClose newPosition) (position initialPlayer : positions)
        then randomMobPositions n positions
        else randomMobPositions (n - 1) (newPosition : positions)



tooClose :: (Float, Float) -> (Float, Float) -> Bool
tooClose (x1, y1) (x2, y2) =
    (x2 - x1)^2 + (y2 - y1)^2 < 80^2

main :: IO ()
main = do
    positions <- randomMobPositions 8 []

    let startingMobs =
            map (\pos -> initialMob { mobPosition = pos }) positions

        startingWorld = initialWorld { mobs = startingMobs }

    play
        (InWindow "Chronobound" (800, 600) (100, 100))
        white
        60
        startingWorld
        drawWorld
        handleInput
        updateWorld