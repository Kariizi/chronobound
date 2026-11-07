module Controller where

import Graphics.Gloss.Interface.Pure.Game
import Model
import Player
import Model (World(heldKeys))

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

updateWorld :: Float -> World -> World 
updateWorld dt world = 
    let 
        p = player world 
        keys = heldKeys world 

        -- Calculating direction (Velocity) 
        direction = calculateVelocity keys
        -- Normalise the direction
        normalisedDirection = normaliseVelocity direction

        -- Get updated player with new velocity
        movingPlayer = p { velocity = normalisedDirection }

        -- Move the updated player 
        movedPlayer = movePlayer dt movingPlayer

    in world { player = movedPlayer }

-- Normalising Movement
normaliseVelocity :: (Float, Float) -> (Float, Float)
normaliseVelocity (vx, vy) = 
    let 
        magnitude = (sqrt(vx^2 + vy^2))
    in if magnitude == 0 
        then (0,0)
        else (vx / magnitude, vy / magnitude)
