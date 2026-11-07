module Mob where
import Model

-- monster movements
moveMob :: Float -> Player -> Mob -> Mob
moveMob dt p mob =
    let
        (mx, my) = mobPosition mob
        (px, py) = position p

        -- Calculate direction towards the player
        dx = px - mx
        dy = py - my

        -- Calculate distance between monster and player
        distance = sqrt (dx*dx + dy*dy)
        stopDistance = playerRadius + mobRadius mob

 in
        if distance <= stopDistance
        then mob
        else 
            let
                -- Calculate how far the monster should move
                travelDistance = min (distance - stopDistance) (mobSpeed mob * dt)

                -- Calculate new position
                newX = mx + (dx / distance) * travelDistance
                newY = my + (dy / distance) * travelDistance

            in mob { mobPosition = (newX, newY) }


       

moveMobs :: Float -> World -> World
moveMobs dt world =
    let
        p = player world
        playerZone = zoneOf (position p)

        -- Only chase if the player is in this mob's zone
        updateMob mob
            | mobZone mob == playerZone = moveMob dt p mob
            | otherwise                 = mob

        oldMobs = mobs world
        newMobs = map updateMob oldMobs
        separatedMobs = resolveAllCollisions oldMobs newMobs
    in
        world { mobs = separatedMobs }


        
-- Check collisions for all monsters
resolveAllCollisions :: [Mob] -> [Mob] -> [Mob]
resolveAllCollisions oldMobs newMobs =
    zipWith checkCollision oldMobs newMobs
    where
        checkCollision oldMob newMob =
            let
                otherMobs = filter (/= oldMob) oldMobs
                overlapping = any (colliding newMob) otherMobs
            in
                if overlapping
                then oldMob
                else newMob

mobCollisionDistance :: Mob -> Mob -> Float
mobCollisionDistance mob1 mob2 =
    mobRadius mob1 + mobRadius mob2

distanceBetweenMobs :: Mob -> Mob -> Float
distanceBetweenMobs mob1 mob2 =
    let
        (x1, y1) = mobPosition mob1
        (x2, y2) = mobPosition mob2
    in
        sqrt ((x2 - x1)^2 + (y2 - y1)^2)


colliding :: Mob -> Mob -> Bool
colliding mob1 mob2 =
    distanceBetweenMobs mob1 mob2 < mobCollisionDistance mob1 mob2

-- Damaging Mob
mobTakeDamage :: Int -> Mob -> Mob 
mobTakeDamage damage mob = 
    let 
        currentHP = mobHealth mob 
        newHP = max 0 (currentHP - damage)
    in mob {mobHealth = newHP}


-- collide

-- attack

-- dead