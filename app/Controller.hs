module Controller where

import Graphics.Gloss.Interface.Pure.Game
import Model
import Player
import Combat


-- Handles keyboard events 

handleInput :: Event -> World -> World 

-- Updating Held Keys: So now it stores what keys are being pressed, and what keys aren't pressed
handleInput (EventKey (Char key) Down _ _) world 
    | key `elem` "wasd" = 
        let 
            currentKeys = heldKeys world 

            newKeys = 
                if (key `elem` currentKeys ) 
                    then currentKeys
                    else key : currentKeys

        
        in world { heldKeys = newKeys }

handleInput (EventKey (Char key) Up _ _) world 
    | key `elem` "wasd" = 
        let 
            currentKeys = heldKeys world 

            newKeys = 
                if (key `elem` currentKeys )
                    -- Goes through list and checks the element isn't equal to key 
                    -- Returns list with keys that isn't equal to key
                    then filter(/= key) currentKeys 
                    else currentKeys

        in world { heldKeys = newKeys }

-- For attacking
handleInput (EventKey (SpecialKey KeySpace) Down _ _) world = 
    let 
        p = player world 
        attackingPlayer = startAttack p 

    in world { player = attackingPlayer }

-- For Dashing
handleInput (EventKey (Char 'f') Down _ _) world = 
    let 
        p = player world 
        keys = heldKeys world
        -- Gets the direction of the player 
        direction = normaliseVelocity (calculateVelocity keys)

        chosenDirection = 
            if direction == (0,0)
                then facingDirection p
                else direction
            
        dashingPlayer = startDash chosenDirection p 

    in world { player = dashingPlayer }

handleInput _ model = model 

-- Calculating the Velocity: For multiple keys
calculateVelocity :: [Char] -> (Float, Float)
calculateVelocity keys = 
    let
        vx
            -- If D is pressed AND A is not pressed, move right (positive X direction)
            | 'd' `elem` keys && not ('a' `elem` keys) = 1
            -- If A is pressed AND D is not pressed, move left (negative X direction)
            | 'a' `elem` keys && not ('d' `elem` keys) = -1
            | otherwise = 0

        vy 
            | 'w' `elem` keys && not ('s' `elem` keys) = 1
            | 's' `elem` keys && not ('w' `elem` keys) = -1
            | otherwise = 0

    in (vx,vy)

-- Normalising Movement
normaliseVelocity :: (Float, Float) -> (Float, Float)
normaliseVelocity (vx, vy) = 
    let 
        magnitude = (sqrt(vx^2 + vy^2))
    in if magnitude == 0 
        then (0,0)
        else (vx / magnitude, vy / magnitude)

updateWorld :: Float -> World -> World 
updateWorld dt world = 
    let 
        p = player world 
        keys = heldKeys world 

        -- Calculating direction (Velocity) 
        direction = calculateVelocity keys
        -- Normalise the direction
        normalisedDirection = normaliseVelocity direction

        -- Gets the direction character faces
        newFacing
            | isDashing p = dashDirection p
            -- Attack direction will be the direction of movement for now
            | normalisedDirection == (0, 0) = facingDirection p
            | otherwise = normalisedDirection

        -- Get updated player with new velocity
        movingPlayer = p 
            { velocity = normalisedDirection
            , facingDirection = newFacing
            }

        -- Move the updated player 
        movedPlayer = 
            if isDashing movingPlayer
                then dashMovement dt movingPlayer
                else movePlayer dt movingPlayer

        -- Update the players timers
        attackUpdatedPlayer = updateAttack dt movedPlayer
        updatedPlayer = updateDash dt attackUpdatedPlayer

    in world { player = updatedPlayer }


