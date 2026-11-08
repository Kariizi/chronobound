-- Everything about the final boss (the Clockmaker): its stats, where it spawns,
-- and later its special moves

module Boss where

import Model

-- The boss's stats, based on initialMob
-- (player melee does 4 damage, so mobHealth / 4 = hits needed)
bossEnemy :: Mob
bossEnemy = initialMob
    { mobSprite = "boss"
    , mobHealth = 120
    , mobMaxHealth = 120
    , mobSpeed = 60
    , mobDamage = 15
    , mobRadius = 30
    , mobAttackRange = 12
    -- Seconds between attacks
    , mobAttackInterval = 1.2
    }

-- Is this mob the boss?
isBoss :: Mob -> Bool
isBoss mob = mobSprite mob == "boss"

-- Where the boss appears, away from the player (who starts at (0, 0))
bossSpawnPosition :: (Float, Float)
bossSpawnPosition = (0, 180)

-- The boss placed in the arena, with its zone matching where it stands
spawnBoss :: Mob
spawnBoss = bossEnemy
    { mobPosition = bossSpawnPosition
    , mobZone = zoneOf bossSpawnPosition
    }

-- BOSS MOVEMENTS:

-- Move the boss towards the player
moveBoss :: Float -> Player -> Mob -> Mob
moveBoss dt p boss =
    let
        (px, py) = position p
        (bx, by) = mobPosition boss

        -- Calculate direction (x and y) + distance 
        dx = px - bx
        dy = py - by
        distance = sqrt (dx * dx + dy * dy)

        -- Distance boss needs to attack
        stopDistance = playerRadius + mobRadius boss + mobAttackRange boss

        -- Gets minimum between how close boss needs to get and distance it can move this frame
        travelDistance = max 0 (min (distance - stopDistance) (mobSpeed boss * dt))

        (newX, newY)
            | distance <= stopDistance = (bx, by)
            | otherwise = 
                -- Normalise direction then times by unit it can move
                ( bx + (dx / distance) * travelDistance
                , by + (dy / distance) * travelDistance
                )

    in boss { mobPosition = (newX, newY) }

-- BOSS ATTACKS: