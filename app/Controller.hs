
module Controller where

import Graphics.Gloss.Interface.Pure.Game
import Model
import Player
import Combat
import Mob

-- Handles keyboard events
handleInput :: Event -> World -> World

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


-- Update the game world every frame
updateWorld :: Float -> World -> World
updateWorld dt world =
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

        -- Move normally or dash
        movedPlayer =
            if isDashing movingPlayer
                then dashMovement dt movingPlayer
                else movePlayer dt movingPlayer

        -- Update attack and dash timers
        attackUpdatedPlayer = updateAttack dt movedPlayer
        dashUpdatedPlayer = updateDash dt attackUpdatedPlayer
        updatedPlayer = updateRewind dt dashUpdatedPlayer

        -- Store updated player in world (update the player in world)
        worldWithPlayer = world { player = updatedPlayer }

        updatedNumbers = updateDamageNumbers dt (damageNumbers world)

        -- Updates the numbers in the world 
        worldWithNumbers = worldWithPlayer
            { damageNumbers = updatedNumbers }

        worldWithMobs = moveMobs dt worldWithNumbers

    -- Update mobs using the new player position
    in recordPlayerSnapshot dt worldWithMobs