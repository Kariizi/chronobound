module Controller where

import Graphics.Gloss.Interface.Pure.Game
import Model
import Player

-- Handles keyboard events 

handleInput :: Event -> World -> World 

-- Updating the players velocity

handleInput (EventKey (Char 'w') Down _ _) world = 
    let 
        -- Retrieves the player from the world (since world contains player)
        -- Retrieve player field from world and bind to p
        p = player world 

        -- Create player with updated velocity 
        newPlayer = p { velocity = (0,1) } 

    in world { player = newPlayer }

handleInput (EventKey (Char 'a') Down _ _) world = 
    let 
        p = player world 
        newPlayer = p { velocity = (-1,0) } 

    in world { player = newPlayer }


handleInput (EventKey (Char 's') Down _ _) world = 
    let 
        p = player world 
        newPlayer = p { velocity = (0,-1) } 

    in world { player = newPlayer }

handleInput (EventKey (Char 'a') Down _ _) world = 
    let 
        p = player world 
        newPlayer = p { velocity = (1,0) } 

    in world { player = newPlayer }

-- Ignores other events 
handleInput _ world = world 


-- Updating players position on game world 
updateWorld :: Float -> World -> World 

updateWorld dt world = 
    let 
        p = player world 
        -- Calls exisiting function to calculate players new position
        movedPlayer = movePlayer dt p

    in world { player = movedPlayer }



handleInput _ model = model 