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
        -- Get the enemy types for current lvl
        types = enemyTypesFor level
        -- Generate random index 
        (index, generator1) = randomR (0, length types - 1) generator
    -- Return the selected enemy and updated random generator
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


-- Waves - Spawn new waves aside from hell level once mobs clear
respawnIfCleared :: World -> World
respawnIfCleared world =
    let
        -- Check whether every mob has been killed
        allMobsDead = null (mobs world)

        -- Only Victoria and Limbo get new waves
        notInHell = currentLevel world /= HellLevel

        inVictoria = currentLevel world == VictoriaLevel

    in
        if allMobsDead && notInHell
            then
                let
                    nextWave =
                        if inVictoria
                            then victoriaWave world + 1
                            else victoriaWave world

                    -- Spawn 3 new mobs in every zone
                    (newWave, newGenerator) =
                        spawnAllZones 3 (currentLevel world) (rng world)

                    -- Strengthen victoria wave
                    toughenedWave =
                        if inVictoria
                            then map (strengthenMob nextWave) newWave
                            else newWave

                in world
                    { mobs = toughenedWave
                    , rng = newGenerator
                    , victoriaWave = nextWave
                    }

            else world


-- Stuck mobs - stop mob from being stuck on building 
unstickMobs :: Walkable -> World -> World
unstickMobs walkable world =
    let
        -- Fix every mob, passing the random generator along
        (fixedMobs, newGenerator) = unstickEachMob walkable (mobs world) (rng world)

    in world { mobs = fixedMobs, rng = newGenerator }


-- Go through the mobs one by one and move any that are stuck
unstickEachMob :: Walkable -> [Mob] -> StdGen -> ([Mob], StdGen)
unstickEachMob _ [] generator =
    -- Base case: no mobs left to check
    ([], generator)

unstickEachMob walkable (mob : remainingMobs) generator =
    let
        -- Check whether this mob is standing somewhere walkable
        isStuck = not (walkable (mobPosition mob))

        -- If it's stuck, find it a new position
        (fixedMob, generator1) =
            if isStuck
                then
                    let
                        (newPosition, newGenerator) =
                            findWalkablePosition walkable (mobZone mob) 20 generator

                    in (mob { mobPosition = newPosition }, newGenerator)

                else (mob, generator)

        -- Check the rest of the mobs using the updated generator
        (fixedRest, generator2) = unstickEachMob walkable remainingMobs generator1

    in (fixedMob : fixedRest, generator2)


-- Keep picking random positions in the zone until one is walkable
findWalkablePosition :: Walkable -> Zone -> Int -> StdGen -> ((Float, Float), StdGen)
findWalkablePosition walkable zone triesLeft generator =
    let
        -- Pick a random position in the zone
        (newPosition, generator1) = randomPosition zone generator

    in
        if walkable newPosition || triesLeft <= 0
            then (newPosition, generator1)
            else findWalkablePosition walkable zone (triesLeft - 1) generator1


-- Increase difficulty of wave
strengthenMob :: Int -> Mob -> Mob
strengthenMob layer mob =
    let
        -- How many layers deeper than layer 1 we are
        layersDeep = max 0 (layer - 1)
        layersDeepFloat = fromIntegral layersDeep
        -- Increase health by 25% per layer
        newHealth = round (fromIntegral (mobMaxHealth mob) * (1 + 0.25 * layersDeepFloat))
        -- Increase damage by 1 per layer
        newDamage = mobDamage mob + layersDeep
        -- Increase speed by 10% per layer
        newSpeed = mobSpeed mob * (1 + 0.1 * layersDeepFloat)
        -- Attack 10% more often per layer (but never faster than every 0.4 seconds)
        newAttackInterval = max 0.4 (mobAttackInterval mob * (1 - 0.1 * layersDeepFloat))
    in mob
        { mobHealth = newHealth
        , mobMaxHealth = newHealth
        , mobDamage = newDamage
        , mobSpeed = newSpeed
        , mobAttackInterval = newAttackInterval
        }
