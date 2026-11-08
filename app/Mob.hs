module Mob where
import Model
import Player (isDashing)
import Boss (isBoss)

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

        updateMob (i, currentMob) =
            let
                -- The boss follows the player into every zone so it never slows down
                mob =
                    if isBoss currentMob
                        then currentMob { mobZone = playerZone }
                        else currentMob
            in case mobAction mob of
                -- Stands still while the warning shows
                WindingUp _ -> mob

                -- Charges in a straight line in the direction it locked in
                Lunging _ (dirX, dirY) ->
                    let (mx, my) = mobPosition mob
                        lunged = mob { mobPosition = ( mx + dirX * mobLungeSpeed * dt
                                                     , my + dirY * mobLungeSpeed * dt ) }
                    in safely mob lunged

                -- Normal chasing towards its slot around the player
                Chasing
                    | mobZone mob == playerZone -> safely mob (moveMob dt (slotAngle i) p mob)
                    | otherwise                 -> safely mob (moveMob (dt * otherZoneSpeedFactor) (slotAngle i) p mob)

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
    && not (isDashing p)

-- Count a mob's cooldown down
tickCooldown :: Float -> Mob -> Mob
tickCooldown dt mob =
    mob { mobAttackCooldown = max 0 (mobAttackCooldown mob - dt) }

-- One mob's attack: returns the updated mob and the damage it dealt
-- Damage only lands while lunging; hitting ends the lunge so it can only hit once
resolveAttack :: Player -> Mob -> (Mob, Int)
resolveAttack p mob =
    case mobAction mob of
        Lunging _ _
            | touchingPlayer && not (isDashing p) ->
                ( mob { mobAction = Chasing, mobAttackCooldown = mobAttackInterval mob }
                , mobDamage mob )
        _ -> (mob, 0)
  where
    touchingPlayer =
        distanceToPlayer p mob <= playerRadius + mobRadius mob + mobAttackRange mob

-- Changing the Mobs Attack:
updateMobAttack :: Float -> Player -> Mob -> Mob 
updateMobAttack dt p mob =
    case mobAction mob of 
        Chasing -> 
            if (mobZone mob == zoneOf (position p)
                && distanceToPlayer p mob <= playerRadius + mobRadius mob + mobAttackRange mob
                && mobAttackCooldown mob <= 0)
                then mob { mobAction = WindingUp mobWindupTime }
                else mob 
            
        WindingUp timeLeft ->
            let
                newWindUp = timeLeft - dt
                (px, py) = position p
                (mx, my) = mobPosition mob
                dx = px - mx
                dy = py - my
                distance = sqrt (dx * dx + dy * dy)
                direction =
                    if distance > 0
                        then (dx / distance, dy / distance)
                        else (0, 0)
            in
                if newWindUp <= 0
                    then mob { mobAction = Lunging mobLungeTime direction }
                    else mob { mobAction = WindingUp newWindUp }
            
        Lunging timeLeft direction ->
            let
                newLungeTime = timeLeft - dt
            in
                if newLungeTime <= 0
                    -- Lunge has finished, return to chasing
                    -- Reset the cooldown before the mob can attack again
                    then mob
                        { mobAction = Chasing
                        , mobAttackCooldown = mobAttackInterval mob
                        }
                    -- Continue lunging in the same direction
                    else mob
                        { mobAction = Lunging newLungeTime direction
                        }

-- All mobs attack. Outside Hell the player loses time; in Hell the trial timer drops
mobAttacks :: Float -> World -> World
mobAttacks dt world =
    let
        p = player world
        (px, py) = position p
        -- Count cooldowns down, then move each mob through chase -> windup -> lunge
        ticked = map (updateMobAttack dt p . tickCooldown dt) (mobs world)
        results = map (resolveAttack p) ticked
        totalDamage = sum (map snd results)
        hitNumbers =
            [ DamageNumber
                { damagePosition = (px, py + playerRadius)
                , damageAmount = dmg
                , damageLifetime = 0.5
                , damageHitsPlayer = True
                , gainedHP = False
                }
            | (_, dmg) <- results, dmg > 0
            ]

        inHell = currentLevel world == HellLevel

        -- Outside Hell hits drain the player's time; in Hell they drain the trial timer
        newPlayer
            | inHell    = p
            | otherwise = p { health = max 0 (health p - totalDamage) }

        newHellTime
            | inHell    = max 0 (hellTimeRemaining world
                                 - fromIntegral totalDamage * hellDamageTimeFactor)
            | otherwise = hellTimeRemaining world
    in
        world
            { mobs = map fst results
            , player = newPlayer
            , hellTimeRemaining = newHellTime
            , damageNumbers = damageNumbers world ++ hitNumbers
            }