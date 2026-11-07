module Combat where

import Model 

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

-- Melee Attack: 