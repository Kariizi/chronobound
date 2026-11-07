-- Defines any player movement, combat etc 

module Player where
import Model 

-- General Upgrade 
applyUpgrade :: Upgrade -> Player -> Player 
applyUpgrade upgrade p = 
    -- Checks upgrade selected
    case upgrade of 
        -- Adjust this later 
        HealthUpgrade amount -> 
            let 
                currentHP = health p 
                maximumHP = maxHealth p 

                newMaxHP = maximumHP + amount
                newHP = currentHP + amount
            in p { health = newHP, maxHealth = newMaxHP }
        SpeedUpgrade  amount -> 
            let 
                currentSpeed = moveSpeed p 

                newMoveSpeed = currentSpeed + amount 
            in p {moveSpeed = newMoveSpeed }
        DamageUpgrade  amount ->
            let
                currentDamage = attackDamage p 
            
                newDamage = currentDamage + amount 
            in p {attackDamage = newDamage }

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

-- Player Damage:
takeDamage :: Int -> Player -> Player 
takeDamage damage p = 
    let 
        -- Retrieves current players health 
        currentHP = health p 
        newHP = max 0 (currentHP - damage)

    in p {health = newHP}

-- Player is Dead
isDead :: Player -> Bool
isDead p = health p == 0 

-- Player Dashing 
isDashing :: Player -> Bool 
isDashing p = dashTimer p > 0 

-- Dash if cooldown = 0, save direction where dash starts
startDash :: (Float, Float) -> Player -> Player 
startDash direction p = 
    if dashCooldownRemaining p > 0
        then p 
        else p { 
            dashTimer = dashDuration
            , dashCooldownRemaining = dashCooldown
            , dashDirection = direction
        }

updateDash :: Float -> Player -> Player 
updateDash dt p = 
    let 
        currentTimer = dashTimer p 
        currentCooldown = dashCooldownRemaining p 

        newTimer = max 0 (currentTimer - dt)
        newCooldown = max 0 (currentCooldown - dt)
    in p { dashTimer = newTimer, dashCooldownRemaining = newCooldown}

dashMovement :: Float -> Player -> Player 
dashMovement dt p = 
    let 
        (x, y) = position p
        (dx, dy) = dashDirection p 

        newX = x + (dx * dashSpeed * dt)
        newY = y + (dy * dashSpeed * dt)

    in p { position = (newX, newY) }

-- For rewind cooldown
updateRewind :: Float -> Player -> Player
updateRewind dt p =
    let
        -- Retrieves current rewind cooldown
        currentCooldown = rewindCooldownRemaining p

        -- Decreases cooldown by dt, preventing negative values
        newCooldown = max 0 (currentCooldown - dt)

    -- Updates player with new rewind cooldown
    in p { rewindCooldownRemaining = newCooldown }


-- Recording players snapshot
recordPlayerSnapshot:: Float -> World -> World 
recordPlayerSnapshot dt world = 
    let 
        p = player world 

        -- Creates new player snapshot
        newSnapshot = PlayerSnapshot 
            { snapshotPosition = position p
            , snapshotHealth = health p 
            , snapshotAge = 0
            }

        -- Takes on snapshot
        ageSnapshot snapshot = 
            snapshot 
                -- Retrieves snapshotAge of snapshot, adds dt
                { snapshotAge = snapshotAge snapshot + dt }

        -- Retrieves all snapshots, and map applies ageSnapshot to every snapshot
        ageHistory = map ageSnapshot (playerHistory world)

        recentHistory =  filter isRecent ageHistory

        isRecent snapshot
            -- Takes snapshot, and compares if age < 2 
            | snapshotAge snapshot <= 2.0 = True
            | otherwise = False

        
        -- Add new snapshot to front of the list
        updatedHistory = newSnapshot : recentHistory 

    in world { playerHistory = updatedHistory }

-- Rewind Functionality
rewindPlayer :: World -> World
rewindPlayer world
    -- Check if rewind is still on cooldown
    | rewindCooldownRemaining (player world) > 0 = world
    | otherwise =
        let
            history = playerHistory world
            p = player world

        in case history of
            -- For empty history return current world
            [] -> world

            -- For history with snapshots
            _ ->
                let
                    -- Gets the final element of list
                    oldestSnapshot = last history

                    rewoundPlayer = p
                        -- Retrieves the snapshot position of oldest snapshot + health
                        { position = snapshotPosition oldestSnapshot
                        , health = snapshotHealth oldestSnapshot

                        -- Start the rewind cooldown
                        , rewindCooldownRemaining = rewindCooldown
                        }

                -- Replaces current player with new player
                -- Clear old snapshots
                in world
                    { player = rewoundPlayer
                    , playerHistory = []
                    }