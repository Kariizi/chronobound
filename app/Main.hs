import Graphics.Gloss.Interface.Pure.Game
import Model
import View
import Controller
import System.Random (randomRIO, newStdGen)
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

-- Takes the starting world so Enter can restart the game after Game Over
handleLevelInput :: World -> Event -> World -> World

-- Handle upgrade selection
handleLevelInput _ event world
    | gameState world == ChoosingUpgrade =
        case event of
            -- Select first upgrade
            EventKey (Char '1') Down _ _ -> chooseUpgrade 1 world

            -- Select second upgrade
            EventKey (Char '2') Down _ _ -> chooseUpgrade 2 world

            -- Select third upgrade
            EventKey (Char '3') Down _ _ -> chooseUpgrade 3 world

            -- Ignore other keys while choosing
            _ -> world

-- Start screen: Enter begins the game
handleLevelInput _ event world
    | gameState world == Menu =
        case event of
            EventKey (SpecialKey KeyEnter) Down _ _ -> world { gameState = Playing }
            _ -> world

-- For Victory or Game Over
handleLevelInput startingWorld event world
    | gameState world /= Playing =
        case event of
            EventKey (SpecialKey KeyEnter) Down _ _ -> startingWorld
            _ -> world

-- Debug keys: jump to a level with HP that belongs there
handleLevelInput _ (EventKey (Char '1') Down _ _) world = debugJump VictoriaLevel 100 world
handleLevelInput _ (EventKey (Char '2') Down _ _) world = debugJump LimboLevel 20 world
handleLevelInput _ (EventKey (Char '3') Down _ _) world = debugJump HellLevel 0 world
handleLevelInput _ event world = handleInput event world

-- Sets the player's HP and enters the level properly (new mobs, Hell trial reset)
-- HP matches the level so levelDependingOnHP doesn't immediately switch back
debugJump :: Level -> Int -> World -> World
debugJump level hp world =
    let
        p = player world
        worldWithHP = world { player = p { health = hp } }

    in enterLevel level worldWithHP

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

-- Enemy character names used when spawning mobs (the animation type is added when drawing)
enemySprites :: [String]
enemySprites =
    [ "bat_ghoul"
    , "cultist"
    , "ghost"
    , "hooded_brute"
    , "shadow_beast"
    , "skeleton"
    , "vampire"
    , "wolf_black"
    , "wolf_brown"
    ]

characters :: [String]
characters =
    [ "player"
    , "player_corrupted"
    , "boss"
    , "bat_ghoul"
    , "cultist"
    , "ghost"
    , "hooded_brute"
    , "shadow_beast"
    , "skeleton"
    , "vampire"
    , "wolf_black"
    , "wolf_brown"
    ]

animationNames :: [String]
animationNames =
    [ character ++ "_" ++ action
    -- Loops through each character in character list
    | character <- characters
    -- Loop through each animation type for the character
    , action <- ["breathe", "walk", "attack", "dash"]
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

    -- Creates generator for mob spawning
    generator <- newStdGen

    -- Initialise the world with spawned mobs 
    let startingWorld = initialWorld { mobs = startingMobs, rng = generator }

    -- Load all three maps
    maps <- Maps
        <$> Victoria.loadVictoria "app/assets/victoria"
        <*> Limbo.loadLimbo "app/assets/limbo"
        <*> Hell.loadHell "app/assets/hell"

    -- Loads animated GIF sprites instead of static PNGs
    sprites <- loadAnimationSet 8 "app/assets/spriteanimated" animationNames

    play
        (InWindow "Chronobound" (round windowWidth, round windowHeight) (100, 100))
        black
        60

        -- Open on the start screen (restarts use startingWorld and skip it)
        (startingWorld { gameState = Menu })

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

                  -- Game Over overlay goes last so it covers everything
                , drawGameOver world

                  -- Victory screen after the boss is defeated
                , drawVictory world

                  -- Upgrade selection screen between Hell layers
                , drawUpgradeScreen world

                  -- Start screen shown when the game first opens
                , drawStartScreen world
                ])

        -- Handle movement, attacks, dash, rewind, level debug keys and restarting
        (handleLevelInput startingWorld)

        -- Update game world every frame
        updateWorld
