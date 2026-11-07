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

        -- Distance mov can be for it to be hit (units from centre)
        -- Square to get squared for comparison  
        hitRadius = meleeHitRadius

    -- Checks if the distance from mob and centre is less than meaning its inside attack 
    in distanceSquared <= hitRadius * hitRadius 

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
        -- Takes mob and checks if it returns true for conditon 
        aliveMobs = filter (\mob -> mobHealth mob >0) updatedMobs

    in world { mobs = aliveMobs }