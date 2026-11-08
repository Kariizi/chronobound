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

        -- Pick a random enemy type for this level
        (enemyType, generator2) = randomEnemy level generator1

        -- Place the chosen enemy type in the zone
        newMob = enemyType
            { mobPosition = newPosition
            , mobZone = zone
            }

    -- Return both the new mob and updated generator
    in (newMob, generator2)


-- ENEMY TYPES
-- Each type is initialMob with different stats; the sprite is chosen per level
-- (player melee does 3 damage, so HP 9 = 3 hits, 20 = 7 hits, 30 = 10 hits)

-- Standard enemy: the original mob stats
normalEnemy :: String -> Mob
normalEnemy sprite = initialMob { mobSprite = sprite }

-- Fast and fragile: rushes in, hits often but weakly, dies quickly
fastEnemy :: String -> Mob
fastEnemy sprite = initialMob
    { mobSprite = sprite
    , mobHealth = 9
    , mobMaxHealth = 9
    , mobSpeed = 140
    , mobDamage = 3
    , mobRadius = 12
    , mobAttackInterval = 0.7
    }

-- Tank: slow and tough, hits hard but rarely
tankEnemy :: String -> Mob
tankEnemy sprite = initialMob
    { mobSprite = sprite
    , mobHealth = 30
    , mobMaxHealth = 30
    , mobSpeed = 50
    , mobDamage = 10
    , mobRadius = 20
    , mobAttackRange = 8
    , mobAttackInterval = 1.6
    }

-- Enemy types that can spawn in each level
-- A type listed twice is twice as likely to be picked
enemyTypesFor :: Level -> [Mob]
enemyTypesFor level =
    case level of
        VictoriaLevel ->
            [ normalEnemy "cultist", normalEnemy "cultist"
            , fastEnemy "wolf_brown"
            , tankEnemy "hooded_brute"
            ]
        LimboLevel ->
            [ normalEnemy "ghost", normalEnemy "ghost"
            , fastEnemy "bat_ghoul"
            , tankEnemy "vampire"
            ]
        HellLevel ->
            [ normalEnemy "shadow_beast", normalEnemy "shadow_beast"
            , fastEnemy "wolf_black"
            , tankEnemy "skeleton"
            ]

-- Pick one random enemy type for a level
randomEnemy :: Level -> StdGen -> (Mob, StdGen)
randomEnemy level generator =
    let
        types = enemyTypesFor level
        (index, generator1) = randomR (0, length types - 1) generator
    in (types !! index, generator1)


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
