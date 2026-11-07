-- Defines any player movement, combat etc 

module Player where
import Model 

-- General Upgrade 
applyUpgrade :: Upgrade -> Player -> Player 
applyUpgrade upgrade p = 
    -- Checks upgrade selected
    case upgrade of 
        HealthUpgrade amount -> 
            let 
                currentHP = health p 
                maximum
            newMaxHP = maxHealth + amount
            newHp = health + amount
        SpeedUpgrade  amount -> 
            newMoveSpeed = speed + amount 
        DamageUpgrade  amount ->
            newDamage = attackDamage + amount 

-- Player Movement: Takes deltatime & player, returns a player
movePlayer :: Float -> Player -> Player 
-- Takes delta time (time for update) and player
movePlayer dt p = 
    -- let defines the local variables to be used
    let 
        -- Retrieves existing position record for player p, binding first value to x and second to y
        -- Same for any others now
        (x, y) = position p 
        (vx, vy) = velocity p 
        speed = moveSpeed p 

        -- Calculate new position
        newX = (x + (vx * speed * dt))
        newY = (y + (vy * speed * dt))

    -- in uses those values defined 
    in p { position = (newX, newY) }

-- Player Upgrades: 
upgradePlayerHealth :: Int -> Player -> Player 

-- Player Damage:
takeDamage :: Int -> Player -> Player 
take takeDamage p = 
    let 
        -- Retrieves current players health 
        currentHP = health p 
        newHP = max 0 (currentHP - damage)

    in p {health = newHP}

-- Player is Dead
isDead :: Player -> Bool
isDead p = health p == 0 

