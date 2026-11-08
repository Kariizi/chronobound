module Spawn where 

import Model 
import System.Random (StdGen, randomR)

-- Generates random position for the mob 

randomPosition :: Zone -> StdGen -> ((Float, Float), StdGen)
randomPosition zone generator = 
    let 
        ((minX, maxX), (minY, maxY)) = 
            case zone of 
                TopLeft ->
                    ((-350, -50), (50, 250))

                TopRight ->
                    ((50, 350), (50, 250))

                BottomLeft ->
                    ((-350, -50), (-250, -50))

                BottomRight ->
                    ((50, 350), (-250, -50))
        -- Generate a random horizontal coordinate
        (x, generator1) = randomR (minX, maxX) generator

        -- Generate a random vertical coordinate using the updated generator
        (y, generator2) = randomR (minY, maxY) generator1

    -- Return the position AND the newest generator
    in ((x, y), generator2)


-- Spawn one mob at a randomly generated position
spawnMob :: Level -> Zone -> StdGen -> (Mob, StdGen)
spawnMob level zone generator =
    let
        -- Generate position and retrieve the new generator
        (newPosition, generator1) = randomPosition zone generator

        -- Choose a starting sprite based on the current level
        sprite =
            case level of
                VictoriaLevel -> "wolf_brown"
                LimboLevel    -> "ghost"
                HellLevel     -> "shadow_beast"

        -- Create a mob using its existing default attributes
        newMob = initialMob
            { mobPosition = newPosition
            , mobZone = zone
            , mobSprite = sprite
            }

    -- Return both the new mob and updated generator
    in (newMob, generator1)


-- Spawn multiple mobs in a specified zone
spawnMobs :: Int -> Level -> Zone -> StdGen -> ([Mob], StdGen)
spawnMobs count level zone generator =
    -- Base case: no more mobs need to be spawned
    if count <= 0
        then ([], generator)

    else
        let
            -- Generate one mob and obtain the updated generator
            (newMob, generator1) = spawnMob level zone generator

            -- Generate the remaining mobs using the updated generator
            (remainingMobs, generator2) =
                spawnMobs (count - 1) level zone generator1

        -- Combine the new mob with the rest of the spawned mobs
        in (newMob : remainingMobs, generator2)


-- Spawn a specified number of mobs in every zone
spawnAllZones :: Int -> Level -> StdGen -> ([Mob], StdGen)
spawnAllZones count level generator =
    let
        -- Generate mobs in the top-left zone
        (topLeftMobs, generator1) =
            spawnMobs count level TopLeft generator

        -- Generate mobs in the top-right zone
        (topRightMobs, generator2) =
            spawnMobs count level TopRight generator1

        -- Generate mobs in the bottom-left zone
        (bottomLeftMobs, generator3) =
            spawnMobs count level BottomLeft generator2

        -- Generate mobs in the bottom-right zone
        (bottomRightMobs, generator4) =
            spawnMobs count level BottomRight generator3

        -- Combine every zone's mobs into a single list
        allMobs =
            topLeftMobs ++ topRightMobs ++ bottomLeftMobs ++ bottomRightMobs

    in (allMobs, generator4)
