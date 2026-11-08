module Combat where

import Model 
import Mob (mobTakeDamage)

-- Checks if the player is attacking: checks if the current attack timer is greater than 0 meaning attacking
isAttacking :: Player -> Bool 
isAttacking p = attackTimer p > 0 

-- Starting Attack
startAttack :: Player -> Player 
startAttack p = 
    if cooldownRemaining p > 0 
        then p
        -- Create new player with active attack and attack cooldown
        else p { attackTimer = 0.15, cooldownRemaining = 0.4 }

-- Updating Attack
updateAttack :: Float -> Player -> Player 
-- deltatime: represents time from last game update
updateAttack dt p = 
    let 
        currentTimer = attackTimer p 
        currentCooldown = cooldownRemaining p 

        newTimer = max 0 (currentTimer - dt)
        newCooldown = max 0 (currentCooldown - dt)
    in p { attackTimer = newTimer, cooldownRemaining = newCooldown }

-- Checking if mob is in range of attack
isMobInRange :: Player -> Mob -> Bool 
isMobInRange p mob = 
    let 
        -- Player position + Facing 
        (px, py) = position p
        (fx, fy) = facingDirection p 

        -- Mob position
        (mx, my) = mobPosition mob 

        -- Gets attack position 
        attackDistance = meleeAttackDistance
        attackX = px + fx * attackDistance 
        attackY = py + fy * attackDistance

        -- Squared distance from attack centre to mob 
        distanceSquared = (attackX - mx)^2 + (attackY - my)^2 

        -- How far the mob centre can be from the swing and still get hit
        -- Adding the mob radius means touching any part of its body counts
        hitRadius = meleeHitRadius + mobRadius mob

    -- Checks if the distance from mob and centre is less than meaning its inside attack 
    in distanceSquared <= hitRadius * hitRadius 

createDamageNumber :: Int -> Mob -> DamageNumber 
createDamageNumber damage mob = 
    DamageNumber 
        { damagePosition = mobPosition mob 
        , damageAmount = damage 
        , damageLifetime = 0.5
        , damageHitsPlayer = False 
        , gainedHP = False
        }

-- Mob Take Damage 
performMeleeAttackMob :: World -> World 
performMeleeAttackMob world =
    let 
        p = player world 
        damage = attackDamage p 
        currentMobs = mobs world 

        attackMob mob = 
            if isMobInRange p mob 
                then mobTakeDamage damage mob 
                else mob 

        -- Apply the attackMob function to every mob in currentMobs
        -- Create updated list with all new mobs
        updatedMobs = map attackMob currentMobs 

        hitMobs = filter (isMobInRange p) currentMobs

        -- Creates damage number for each mob hit
        newDamageNumbers = map (createDamageNumber damage) hitMobs
        -- Combines eisting and new damage numbers
        allDamageNumbers = damageNumbers world ++ newDamageNumbers

        -- Takes mob and checks if it returns true for conditon 
        aliveMobs = filter (\mob -> mobHealth mob >0) updatedMobs

        -- Get number of dead mobs
        kills = length(updatedMobs) - length(aliveMobs)

        -- For the Hell Level:
        currentHellKills = hellKills world 
        newHellKills = 
            if currentLevel world == HellLevel
                then currentHellKills + kills
                else currentHellKills

        -- For healing
        timePerKill =
            if currentLevel world == LimboLevel
                then 15
                else 10
        timeGained = timePerKill * kills 
        -- Health SHOULDNT increase for hell
        newHealth = 
            if currentLevel world == HellLevel
                then health p
                else min (maxHealth p) (health p + timeGained)

        updatedPlayer = p {health = newHealth}
        
        actualGain = newHealth - health p
        (px, py) = position p
        gainNumbers =
            [ DamageNumber
                { damagePosition = (px, py + 30)
                , damageAmount = actualGain
                , damageLifetime = 0.8
                , damageHitsPlayer = False
                , gainedHP = True
                }
            | actualGain > 0
            ]
        

    in world { mobs = aliveMobs , damageNumbers = allDamageNumbers ++ gainNumbers, player = updatedPlayer, hellKills = newHellKills}

-- Attacking Mob = Health Gain