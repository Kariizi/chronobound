import Graphics.Gloss.Interface.IO.Game
import Model
import View
import Controller
import System.Random (randomRIO, newStdGen)
import qualified Victoria
import qualified Limbo
import qualified Hell
import MapArt
import Audio (setupMusic, updateMusic)

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

-- Check whether a position falls inside a house sprite
houseSpriteCovers :: Victoria.Victoria -> Float -> Float -> Bool
houseSpriteCovers v mx my =
    any coversHouse nearbyTiles
  where
    -- Convert map position into approximate tile coordinates
    i = (mx / 16 + my / 8) / 2
    j = (my / 8 - mx / 16) / 2

    -- Check nearby tiles for houses
    nearbyTiles =
        [ (a, b)
        | a <- [floor i - 6 .. floor i + 6]
        , b <- [floor j - 6 .. floor j + 6]
        ]

    coversHouse (a, b) =
        case Victoria.tileAt v (a, b) of
            Victoria.Tile _ (Just (Victoria.House style)) ->
                let
                    -- Position where the house sprite is drawn
                    hx = fromIntegral (a - b) * 16
                    hy = fromIntegral (a + b) * 8

                    -- Height of each house sprite
                    houseHeight = case style of
                        Victoria.RedBrick -> 70
                        Victoria.Stucco   -> 78
                        Victoria.Soot     -> 62

                in
                    mx >= hx - 16 &&
                    mx <= hx + 16 &&
                    my >= hy + 16 - houseHeight &&
                    my <= hy + 16

            _ -> False


-- Points at the feet, relative to the sprite centre, in world pixels.
-- dy is how far below the centre the feet are.
footprint :: [(Float, Float)]
footprint = [(0, 0)]

-- Is this one world point free (not water, empty air or under a house)?
victoriaPointOk :: Victoria.Victoria -> (Float, Float) -> Bool
victoriaPointOk v (fx, fy) =
    let (camX, camY) = Victoria.startView v
        mx = fx / mapZoom + camX
        my = camY - fy / mapZoom
        i  = (mx / 16 + my / 8) / 2
        j  = (my / 8 - mx / 16) / 2
    in case Victoria.tileAt v (floor i, floor j) of
         Victoria.Void          -> False
         Victoria.Tile ground _ ->
           ground /= Victoria.Water && not (Victoria.houseCovers v (mx, my))

-- The player or a mob can stand here if all their foot points are free
victoriaWalkable :: Victoria.Victoria -> Walkable
victoriaWalkable v (wx, wy) =
    all (victoriaPointOk v) [ (wx + dx, wy + dy) | (dx, dy) <- footprint ]


-- Turn a map's "blocked" check (in map pixels) into a Walkable (in world pixels)
walkableOn :: (Float, Float) -> ((Float, Float) -> Bool) -> Walkable
walkableOn (camX, camY) blocked (wx, wy) =
    not (any blocked [ ((wx + dx) / mapZoom + camX, camY - (wy + dy) / mapZoom)
                     | (dx, dy) <- footprint ])

walkableFor :: Maps -> Level -> Walkable
walkableFor ms VictoriaLevel = victoriaWalkable (victoriaMap ms)
walkableFor ms HellLevel     = let m = hellMap ms in walkableOn (Hell.startView m) (Hell.hellBlocked m)
walkableFor ms LimboLevel    = let m = limboMap ms in walkableOn (Limbo.startView m) (Limbo.limboBlocked m)


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

-- Handles Pauses
handleLevelInput _ (EventKey (SpecialKey KeyEsc) Down _ _) world 
    | gameState world == Playing =
        world
            { gameState = Paused
            , heldKeys = []
            }
-- Resume the game when ESC is pressed
handleLevelInput _ (EventKey (SpecialKey KeyEsc) Down _ _) world
    | gameState world == Paused =
        world { gameState = Playing }

-- Ignore other inputs while paused
handleLevelInput _ _ world
    | gameState world == Paused = world

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

handleLevelInput _ event world = handleInput event world

zoneBounds :: Zone -> (Float, Float, Float, Float)
zoneBounds TopLeft     = (-maxX, -lineMargin,  lineMargin,  maxY)
zoneBounds TopRight    = ( lineMargin,  maxX,  lineMargin,  maxY)
zoneBounds BottomLeft  = (-maxX, -lineMargin, -maxY, -lineMargin)
zoneBounds BottomRight = ( lineMargin,  maxX, -maxY, -lineMargin)

maxX, maxY :: Float
maxX = worldHalfWidth  - edgeMargin
maxY = worldHalfHeight - edgeMargin

randomMobPositionsIn :: Walkable -> Zone -> Int -> [(Float, Float)] -> IO [(Float, Float)]
randomMobPositionsIn _ _ 0 positions = return positions

randomMobPositionsIn walkable zone n positions = do
    let (x0, x1, y0, y1) = zoneBounds zone
    x <- randomRIO (x0, x1)
    y <- randomRIO (y0, y1)

    let newPosition = (x, y)

    if not (walkable newPosition) || any (tooClose newPosition) (position initialPlayer : positions)
        then randomMobPositionsIn walkable zone n positions
        else randomMobPositionsIn walkable zone (n - 1) (newPosition : positions)

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
spawnZoneMobs :: Walkable -> Int -> Zone -> IO [Mob]
spawnZoneMobs walkable n zone = do
    positions <- randomMobPositionsIn walkable zone n []
    mapM makeMob positions
  where
    makeMob pos = do
        k <- randomRIO (0, length enemySprites - 1)
        return initialMob
            { mobPosition = pos
            , mobZone     = zone
            , mobSprite   = enemySprites !! k
            }


main :: IO ()
main = do

    -- Load all three maps
    maps <- Maps
        <$> Victoria.loadVictoria "app/assets/victoria"
        <*> Limbo.loadLimbo "app/assets/limbo"
        <*> Hell.loadHell "app/assets/hell"

    startingMobs <- concat <$> mapM (spawnZoneMobs (walkableFor maps VictoriaLevel) 2) [minBound .. maxBound]
    generator <- newStdGen
    let startingWorld = initialWorld { mobs = startingMobs, rng = generator }

    -- Loads animated GIF sprites instead of static PNGs
    sprites <- loadAnimationSet 8 "app/assets/spriteanimated" animationNames

    -- Start audio and load the background music (Nothing if there is no audio device)
    audio <- setupMusic "app/assets/music"

    -- playIO instead of play so the update can start and stop music
    playIO
        (InWindow "Chronobound" (round windowWidth, round windowHeight) (100, 100))
        black
        60

        -- Open on the start screen (restarts use startingWorld and skip it)
        (startingWorld { gameState = Menu })

        -- Draw game world
        (\world -> return $
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

                -- Pause menu overlay
                , drawPauseScreen world
                ])

        -- Handle movement, attacks, dash, rewind and restarting
        (\event world -> return (handleLevelInput startingWorld event world))

        -- Update game world every frame, then switch music if the scene changed
        (\dt world -> do
            let newWorld = updateWorld (walkableFor maps (currentLevel world)) dt world
            mapM_ (\(music, musicPlayer) -> updateMusic music musicPlayer newWorld) audio
            return newWorld)