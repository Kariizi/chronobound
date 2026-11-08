module Mob where
import Model

--- Move one mob towards its slot on the ring around the player
moveMob :: Float -> Float -> Player -> Mob -> Mob
moveMob dt slotAngle p mob =
    let
        (mx, my) = mobPosition mob
        (px, py) = position p

        -- Closest the mob's centre may get to the player's centre
        stopDistance = playerRadius + mobRadius mob

        -- The spot on the ring this mob is aiming for
        targetX = px + cos slotAngle * stopDistance
        targetY = py + sin slotAngle * stopDistance

        -- Direction and distance to that spot (not to the player's centre)
        dx = targetX - mx
        dy = targetY - my
        distance = sqrt (dx*dx + dy*dy)

        travelDistance = min distance (mobSpeed mob * dt)

        (stepX, stepY)
            | distance == 0 = (mx, my)
            | otherwise     = ( mx + dx / distance * travelDistance
                              , my + dy / distance * travelDistance )

        -- If the step would enter the player, slide out to the player's edge
        ox = stepX - px
        oy = stepY - py
        fromPlayer = sqrt (ox*ox + oy*oy)

        (finalX, finalY)
            | fromPlayer < stopDistance && fromPlayer > 0 =
                ( px + ox / fromPlayer * stopDistance
                , py + oy / fromPlayer * stopDistance )
            | otherwise = (stepX, stepY)
    in
        mob { mobPosition = (finalX, finalY) }
       

moveMobs :: Walkable -> Float -> World -> World
moveMobs walkable dt world =
    let
        p = player world
        playerZone = zoneOf (position p)

        slotAngle i = fromIntegral (i :: Int) * 2.39996

        -- Keep the mob where it was if its new spot isn't walkable
        safely mob moved =
            moved { mobPosition = resolveMove walkable (mobPosition mob) (mobPosition moved) }

        updateMob (i, mob)
            | mobZone mob == playerZone = safely mob (moveMob dt (slotAngle i) p mob)
            | otherwise                 = safely mob (moveMob (dt * otherZoneSpeedFactor) (slotAngle i) p mob)

        oldMobs = mobs world
        newMobs = map updateMob (zip [0 ..] oldMobs)
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


-- Distance between the centres of a mob and the player
distanceToPlayer :: Player -> Mob -> Float
distanceToPlayer p mob =
    let (mx, my) = mobPosition mob
        (px, py) = position p
    in sqrt ((px - mx)^2 + (py - my)^2)

-- Can this mob hit the player right now?
canHitPlayer :: Player -> Mob -> Bool
canHitPlayer p mob =
    mobZone mob == zoneOf (position p)
    && mobAttackCooldown mob <= 0
    && distanceToPlayer p mob <= playerRadius + mobRadius mob + mobAttackRange mob

-- Count a mob's cooldown down
tickCooldown :: Float -> Mob -> Mob
tickCooldown dt mob =
    mob { mobAttackCooldown = max 0 (mobAttackCooldown mob - dt) }

-- One mob's attack: returns the updated mob and the damage it dealt
resolveAttack :: Player -> Mob -> (Mob, Int)
resolveAttack p mob
    | canHitPlayer p mob = (mob { mobAttackCooldown = mobAttackInterval mob }, mobDamage mob)
    | otherwise          = (mob, 0)

-- All mobs attack, and the player loses the total damage
mobAttacks :: Float -> World -> World
mobAttacks dt world =
    let
        p = player world
        (px, py) = position p
        ticked = map (tickCooldown dt) (mobs world)
        results = map (resolveAttack p) ticked
        totalDamage = sum (map snd results)
        hitNumbers =
            [ DamageNumber
                { damagePosition = (px, py + playerRadius)
                , damageAmount = dmg
                , damageLifetime = 0.5
                , damageHitsPlayer = True
                }
            | (_, dmg) <- results, dmg > 0
            ]
    in
        world
            { mobs = map fst results
            , player = p { health = max 0 (health p - totalDamage) }
            , damageNumbers = damageNumbers world ++ hitNumbers
            }