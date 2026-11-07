module View where

import Graphics.Gloss
import Model
import Combat (isAttacking)

-- Drawing the world

drawWorld :: World -> Picture 
drawWorld world = 
    let 
        -- Retrieve the Player 
        p = player world

        -- Extract player coordinated
        (x, y) = position p

        -- Retrieve the player's facing direction
        (fx, fy) = facingDirection p

        -- Draw player as circle 
        -- view: draw the circle at the model's position
        playerPicture = translate x y (color green (circleSolid 20))

         -- How far in front of the player the attack appears
        attackDistance = 40

        -- Calculate attack position using facing direction
        attackX = x + fx * attackDistance
        attackY = y + fy * attackDistance

        -- Draw attack indicator only while attacking
        attackPicture =
            if isAttacking p
                then translate attackX attackY
                    (color orange (circleSolid 15))
                else Blank

    -- Render both the player and their attack
    in pictures [playerPicture, attackPicture]