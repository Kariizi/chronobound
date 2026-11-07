import Graphics.Gloss.Interface.Pure.Game
import Model
import View
import Controller
import System.Random (randomRIO)
import qualified Victoria
import qualified Limbo
import qualified Hell
import MapArt

-- Which map is showing. Temporary: press 1, 2 or 3 to switch while we integrate the maps.
data Level = VictoriaLevel | LimboLevel | HellLevel

data Maps = Maps
    { victoriaMap :: Victoria.Victoria
    , limboMap    :: Limbo.Limbo
    , hellMap     :: Hell.Hell
    }

-- The map art is tiny pixel art, so it is drawn at this zoom
mapZoom :: Float
mapZoom = 2

drawLevel :: Maps -> Level -> Picture
drawLevel ms level =
    let view = (windowWidth / mapZoom, windowHeight / mapZoom)
        picture = case level of
            VictoriaLevel -> let m = victoriaMap ms in Victoria.drawVictoria m view (Victoria.startView m)
            LimboLevel    -> let m = limboMap ms    in Limbo.drawLimbo m view (Limbo.startView m)
            HellLevel     -> let m = hellMap ms     in Hell.drawHell m view (Hell.startView m)
    in scale mapZoom mapZoom picture

handleLevelInput :: Event -> (Level, World) -> (Level, World)
handleLevelInput (EventKey (Char '1') Down _ _) (_, world) = (VictoriaLevel, world)
handleLevelInput (EventKey (Char '2') Down _ _) (_, world) = (LimboLevel, world)
handleLevelInput (EventKey (Char '3') Down _ _) (_, world) = (HellLevel, world)
handleLevelInput event (level, world) = (level, handleInput event world)

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

    maps <- Maps
        <$> Victoria.loadVictoria "app/assets/victoria"
        <*> Limbo.loadLimbo "app/assets/limbo"
        <*> Hell.loadHell "app/assets/hell"

    sprites <- loadArtSet "app/assets/sprites"
        ["monster_1", "monster_2", "monster_3",
         "vampire_1", "vampire_2", "vampire_3",
         "werewolf_1", "werewolf_2", "werewolf_3",
         "normal", "scary"
        ]

    play
        (InWindow "Chronobound" (round windowWidth, round windowHeight) (100, 100))
        black
        60
        (VictoriaLevel, startingWorld)
        (\(level, world) -> pictures [drawLevel maps level, drawWorld sprites world])
        handleLevelInput
        (\dt (level, world) -> (level, updateWorld dt world))