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




-- Points around the feet, relative to the sprite centre, in world pixels.
-- Tune these to fit the sprite: dy is how far below centre the feet are.
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
    
    

    maps <- Maps
        <$> Victoria.loadVictoria "app/assets/victoria"
        <*> Limbo.loadLimbo "app/assets/limbo"
        <*> Hell.loadHell "app/assets/hell"

    startingMobs <- concat <$> mapM (spawnZoneMobs (walkableFor maps VictoriaLevel) 2) [minBound .. maxBound]
    let startingWorld = initialWorld { mobs = startingMobs }

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
        (VictoriaLevel, startingWorld)
        (\(level, world) ->
        let
            -- Retrieves player's position
            (px, py) = position (player world)

        in pictures
            [ -- Draw map relative to player position
            drawLevel maps level (px, py)

            -- Shift game layer in opposite direction
            , translate (-px) (-py) (drawWorld sprites world)
                        
            -- Draw HUD without camera translation
            , drawHUD world
            ])
        handleLevelInput
                (\dt (level, world) -> (level, updateWorld (walkableFor maps level) dt world))