module Controller where

import Graphics.Gloss.Interface.Pure.Game
import Model
import Player
import Combat
import Mob
import Spawn (spawnAllZones)

-- Handles keyboard events
handleInput :: Event -> World -> World

-- Ignore all keyboard input after Game Over
handleInput _ world
    | gameState world /= Playing = world

-- Updating held keys for WASD movement
handleInput (EventKey (Char key) Down _ _) world
    | key `elem` "wasd" =
        let
            currentKeys = heldKeys world

            newKeys =
                if key `elem` currentKeys
                    then currentKeys
                    else key : currentKeys

        in world { heldKeys = newKeys }

-- Releasing WASD keys
handleInput (EventKey (Char key) Up _ _) world
    | key `elem` "wasd" =
        let
            currentKeys = heldKeys world

            newKeys =
                if key `elem` currentKeys
                    then filter (/= key) currentKeys
                    else currentKeys

        in world { heldKeys = newKeys }

-- Melee attacking with Space
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world 
    -- Checks if attack is on cooldown, if so return original world
    | cooldownRemaining p > 0 = world 
    -- Performs attack: worldWithAttack updates player, performMeleeAttackMob updates mobs HP  
    | otherwise = performMeleeAttackMob worldWithAttack 
    where 
        p = player world 
        attackingPlayer = startAttack p 
        -- Replaces the current player with attacking player
        worldWithAttack = world { player = attackingPlayer }

-- Dashing with F
handleInput (EventKey (Char 'f') Down _ _) world =
    let
        p = player world
        keys = heldKeys world

        -- Calculate movement direction
        direction = normaliseVelocity (calculateVelocity keys)

        -- Use current movement direction or previous facing direction
        chosenDirection =
            if direction == (0, 0)
                then facingDirection p
                else direction

        -- Start dash
        dashingPlayer = startDash chosenDirection p

    in world { player = dashingPlayer }

-- Rewind with R: Current world is passed to rewindPlayer 
handleInput (EventKey (Char 'r') Down _ _) world =
    rewindPlayer world

-- Ignore other keyboard events
handleInput _ world = world


-- Calculate velocity from held WASD keys
calculateVelocity :: [Char] -> (Float, Float)
calculateVelocity keys =
    let
        vx
            | 'd' `elem` keys && not ('a' `elem` keys) = 1
            | 'a' `elem` keys && not ('d' `elem` keys) = -1
            | otherwise = 0

        vy
            | 'w' `elem` keys && not ('s' `elem` keys) = 1
            | 's' `elem` keys && not ('w' `elem` keys) = -1
            | otherwise = 0

    in (vx, vy)


-- Normalise velocity to prevent faster diagonal movement
normaliseVelocity :: (Float, Float) -> (Float, Float)
normaliseVelocity (vx, vy) =
    let
        magnitude = sqrt (vx^2 + vy^2)

    in if magnitude == 0
        then (0, 0)
        else (vx / magnitude, vy / magnitude)

-- Level Functionality: Switching between Levels and Entering Dimensions 
levelDependingOnHP :: Level -> Int -> Level 
levelDependingOnHP level health 
    | level == VictoriaLevel && health < 30 = LimboLevel
    | level == LimboLevel && health <= 0 = HellLevel
    | level == LimboLevel && health > 40 = VictoriaLevel
    | otherwise = level 

-- Hell Level Functionality 
updateHellTimer :: Float -> World -> World 
updateHellTimer dt world =
    let 
        inHell = currentLevel world == HellLevel
        currentTime = hellTimeRemaining world

        newTime = 
            if inHell
                then max 0 (currentTime - dt)
                else currentTime

    -- Store the updated trial time in the world
    in world { hellTimeRemaining = newTime }

-- Enter a new dimension
enterLevel :: Level -> World -> World
enterLevel newLevel world =
    let
        -- Retrieve the player
        p = player world

        -- Reset the player's position
        updatedPlayer = p { position = (0, 0) }

        -- Generate new mobs using the world's current random generator
        -- 3 mobs per zone = 12 mobs in total
        (newMobs, updatedGenerator) =
            spawnAllZones 3 newLevel (rng world)

        -- Check whether we're entering Hell
        enteringHell = newLevel == HellLevel

        -- Starting a new Hell run from another dimension
        startingHell = enteringHell && currentLevel world /= HellLevel

        -- Reset layer only when entering Hell from another dimension
        newHellLayer =
            if startingHell
                then 1
                else hellLayer world

        -- Reset the timer only when entering Hell
        newHellTime =
            if enteringHell
                then hellTrialDuration + bonusHellTime world
                else hellTimeRemaining world

        -- Reset Hell kills only when entering Hell
        newHellKills =
            if enteringHell
                then 0
                else hellKills world

    in world
        { currentLevel = newLevel
        , player = updatedPlayer

        -- Replace enemies from the previous dimension
        , mobs = newMobs

        -- Save the generator returned by spawnAllZones
        , rng = updatedGenerator

        -- Clear previous dimension's information
        , playerHistory = []
        , projectiles = []
        , damageNumbers = []
        , rewindEffectTimer = 0

        -- Store Hell trial values
        , hellTimeRemaining = newHellTime
        , hellKills = newHellKills
        , hellLayer = newHellLayer
        , bonusHellTime =
            if enteringHell
                then 0
                else bonusHellTime world
        }


updateDamageNumbers :: Float -> [DamageNumber] -> [DamageNumber]
updateDamageNumbers dt numbers =
    let
        updateOneNumber number =
            let
                (x, y) = damagePosition number
                floatSpeed = 60

                newY = y + (floatSpeed * dt)
                newLifetime = max 0 (damageLifetime number - dt)

            in number
                { damagePosition = (x, newY)
                , damageLifetime = newLifetime
                }

        updatedNumbers = map updateOneNumber numbers
        activeNumbers = filter isActive updatedNumbers

        isActive number
            | damageLifetime number > 0 = True
            | otherwise = False

    in activeNumbers

-- FOR UPGRADING
hellUpgradeSelection :: [Upgrade]
hellUpgradeSelection =
    [ TimeUpgrade 15
    , DamageUpgrade 2
    , SpeedUpgrade 40
    ]

-- Applies the chosen upgrade to the player or Hell timer
applyHellUpgrade :: Upgrade -> World -> World
applyHellUpgrade upgrade world =
    let
        p = player world
    in case upgrade of
        -- Store extra time for the next Hell layer
        TimeUpgrade extraTime ->
            world
                { bonusHellTime = bonusHellTime world + extraTime }
        -- Increase the player's attack damage
        DamageUpgrade extraDamage ->
            world
                { player = p
                    { attackDamage = attackDamage p + extraDamage }
                }
        -- Increase the player's movement speed
        SpeedUpgrade extraSpeed ->
            world
                { player = p
                    { moveSpeed = moveSpeed p + extraSpeed }
                }
        -- Existing health upgrade, kept for compatibility
        HealthUpgrade extraHealth ->
            world
                { player = p
                    { maxHealth = maxHealth p + extraHealth
                    , health = health p + extraHealth
                    }
                }


-- Choose an upgrade and move to the next Hell layer
chooseUpgrade :: Int -> World -> World
chooseUpgrade choice world =
    let
        -- Retrieve the available upgrades
        choices = upgradeChoices world

        -- Convert the selected key to a list index
        index = choice - 1

        -- Check whether the selected upgrade is valid
        validChoice = index >= 0 && index < length choices

    in
        if gameState world /= ChoosingUpgrade || not validChoice
            then world

        else
            let
                -- Retrieve the selected upgrade
                selectedUpgrade = choices !! index

                -- Apply the selected upgrade
                upgradedWorld = applyHellUpgrade selectedUpgrade world

                -- Move to the next Hell layer
                nextLayer = hellLayer upgradedWorld + 1

                -- Update the layer and resume gameplay
                updatedWorld = upgradedWorld
                    { hellLayer = nextLayer
                    , gameState = Playing
                    , upgradeChoices = []
                    , heldKeys = []
                    }

            -- Enter the next Hell layer
            in enterLevel HellLevel updatedWorld


-- FOR HELL: CHECKS IF YOU WIN OR LOSE
checkHellTrial :: World -> World 
checkHellTrial world = 
    let 
        inHell = currentLevel world == HellLevel
        -- Get current hell level
        currentLayer = hellLayer world
        -- Check the kill requirement for the current Hell layer
        trialWon =
            hellKills world >= requiredKillsFor (hellLayer world)
        trialLost = hellTimeRemaining world <= 0
    in
        if not inHell
            then world 
        else if trialWon
            then 
                -- Final layer completed
                if currentLayer == finalHellLayer
                    then world { gameState = Victory }
                -- Move to the next layer
                else
                    world
                        { gameState = ChoosingUpgrade
                        , upgradeChoices = hellUpgradeSelection
                        , heldKeys = []
                        }
        -- Trial failed
        else if trialLost
            then world { gameState = GameOver }
        -- Trial ongoing
        else world

-- Updates the world at every frame
-- walkable says which world positions are walkable on the current map
updateWorld :: Walkable -> Float -> World -> World
updateWorld walkable dt world
    -- Keep the clock running on the menu and pause screen so their text can pulse
    | gameState world == Menu || gameState world == Paused =
        world { worldTime = worldTime world + dt }
    
    -- Return world with no updates
    | gameState world /= Playing = world
    | otherwise = updatePlayingWorld walkable dt world

updatePlayingWorld :: Walkable -> Float -> World -> World 
updatePlayingWorld walkable dt world = 
    let 
        p = player world
        keys = heldKeys world

        -- Calculate movement direction
        direction = calculateVelocity keys
        normalisedDirection = normaliseVelocity direction

        -- Determine which direction the player faces
        newFacing
            | isDashing p = dashDirection p
            | normalisedDirection == (0, 0) = facingDirection p
            | otherwise = normalisedDirection

        -- Update player's velocity and facing direction
        movingPlayer = p
            { velocity = normalisedDirection
            , facingDirection = newFacing
            }

        -- Move normally or dash, then stop at walls
        rawMovedPlayer =
            if isDashing movingPlayer
                then dashMovement dt movingPlayer
                else movePlayer dt movingPlayer

        movedPlayer = keepWalkable walkable movingPlayer rawMovedPlayer

        -- Update attack and dash timers
        attackUpdatedPlayer = updateAttack dt movedPlayer
        dashUpdatedPlayer = updateDash dt attackUpdatedPlayer
        rewindUpdatedPlayer = updateRewind dt dashUpdatedPlayer
        updatedPlayer = drainTime dt rewindUpdatedPlayer

        -- Store updated player in world (update the player in world)
        worldWithPlayer = world { player = updatedPlayer }

        updatedNumbers = updateDamageNumbers dt (damageNumbers world)

        -- Updates the numbers in the world 
        worldWithNumbers = worldWithPlayer
            { damageNumbers = updatedNumbers }

        worldWithMobs = mobAttacks dt (moveMobs walkable dt worldWithNumbers)

        -- Update world with snapshots 
        worldWithSnapshot = recordPlayerSnapshot dt worldWithMobs

        -- Determine dimension player should be in 
        newLevel = levelDependingOnHP
            (currentLevel worldWithSnapshot)
            (health (player worldWithSnapshot))

        worldWithLevel =
            -- Compare new level with current
            if newLevel /= currentLevel worldWithSnapshot
                -- If different enter the new dimension and reset level-specific data
                then enterLevel newLevel worldWithSnapshot
                -- Otherwise keep the existing world unchanged
                else worldWithSnapshot
        
        -- Update Hell's countdown after handling dimension changes
        worldWithHellTimer = updateHellTimer dt worldWithLevel

        -- Check whether Death's Trial has ended
        worldWithTrialResult = checkHellTrial worldWithHellTimer

    -- Update mobs using the new player position
    in worldWithTrialResult
        { worldTime = worldTime world + dt
        -- Fade out the rewind effect
        , rewindEffectTimer = max 0 (rewindEffectTimer worldWithTrialResult - dt)
        }