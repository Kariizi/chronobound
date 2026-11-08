import Graphics.Gloss.Interface.Pure.Game
import Model
import View
import Controller
import System.Random (randomRIO)
import qualified Victoria
import qualified Limbo
import qualified Hell
import MapArt

data Maps = Maps
    { victoriaMap :: Victoria.Victoria
    , limboMap    :: Limbo.Limbo
    , hellMap     :: Hell.Hell
    }

-- The map art is tiny pixel art, so it is drawn at this zoom
mapZoom :: Float
mapZoom = 2

-- Draw the current level using the player's position as the camera
drawLevel :: Maps -> Level -> (Float, Float) -> Picture
drawLevel ms level (px, py) =
    let
        view = (windowWidth / mapZoom, windowHeight / mapZoom)

        -- Calculate camera position relative to the level's starting point
        camFrom (sx, sy) =
            ( sx + px / mapZoom
            , sy - py / mapZoom
            )

        picture = case level of
            VictoriaLevel ->
                let
                    m = victoriaMap ms
                in Victoria.drawVictoria m view (camFrom (Victoria.startView m))

            LimboLevel ->
                let
                    m = limboMap ms
                in Limbo.drawLimbo m view (camFrom (Limbo.startView m))

            HellLevel ->
                let
                    m = hellMap ms
                in Hell.drawHell m view (camFrom (Hell.startView m))

    in scale mapZoom mapZoom picture

handleLevelInput :: Event -> World -> World
handleLevelInput (EventKey (Char '1') Down _ _) world = world { currentLevel = VictoriaLevel }
handleLevelInput (EventKey (Char '2') Down _ _) world = world { currentLevel = LimboLevel }
handleLevelInput (EventKey (Char '3') Down _ _) world = world { currentLevel = HellLevel }
handleLevelInput event world = handleInput event world 

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

-- Enemy animation names used when spawning mobs
enemySprites :: [String]
enemySprites =
    [ "bat_ghoul_breathe"
    , "cultist_breathe"
    , "ghost_breathe"
    , "hooded_brute_breathe"
    , "shadow_beast_breathe"
    , "skeleton_breathe"
    , "vampire_breathe"
    , "wolf_black_breathe"
    , "wolf_brown_breathe"
    ]

-- Spawns n mobs in a zone, each with a random enemy sprite
spawnZoneMobs :: Int -> Zone -> IO [Mob]
spawnZoneMobs n zone = do
    positions <- randomMobPositionsIn zone n []
    mapM makeMob positions
  where
    makeMob pos = do
    -- Randomly selects an enemy sprite
        k <- randomRIO (0, length enemySprites - 1)
        -- Generates a random animation offset between 0 and 1 second
        offset <- randomRIO (0.0, 1.0)
        return initialMob
            { mobPosition = pos
            , mobZone = zone
            , mobSprite = enemySprites !! k
            , mobAnimOffset = offset
            }


main :: IO ()
main = do
    -- Spawn mobs in each zone
    startingMobs <- concat <$> mapM (spawnZoneMobs 2) [minBound .. maxBound]

    -- Initialise the world with spawned mobs
    let startingWorld = initialWorld { mobs = startingMobs }

    -- Load all three maps
    maps <- Maps
        <$> Victoria.loadVictoria "app/assets/victoria"
        <*> Limbo.loadLimbo "app/assets/limbo"
        <*> Hell.loadHell "app/assets/hell"

    -- Loads animated GIF sprites instead of static PNGs
    sprites <- loadAnimationSet 8 "app/assets/spriteanimated"
        [ "player_breathe"
        , "player_corrupted_breathe"
        , "boss_breathe"
        , "bat_ghoul_breathe"
        , "cultist_breathe"
        , "ghost_breathe"
        , "hooded_brute_breathe"
        , "shadow_beast_breathe"
        , "skeleton_breathe"
        , "vampire_breathe"
        , "wolf_black_breathe"
        , "wolf_brown_breathe"
        ]

    play
        (InWindow "Chronobound" (round windowWidth, round windowHeight) (100, 100))
        black
        60
        
        startingWorld

        -- Draw game world
        (\world ->
            let
                -- Retrieves player's position
                (px, py) = position (player world)

            in pictures
                [ -- Draw the map based on the level stored in World
                  drawLevel maps (currentLevel world) (px, py)

                  -- Shift game layer in opposite direction
                , translate (-px) (-py) (drawWorld sprites world)

                  -- Draw HUD without camera translation
                , drawHUD world
                ])

        -- Handle movement, attacks, dash, rewind and level debug keys
        handleLevelInput

        -- Update game world every frame
        updateWorld
