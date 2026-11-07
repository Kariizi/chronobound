import Model
import View
import Controller
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