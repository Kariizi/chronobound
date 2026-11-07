module View where

import Graphics.Gloss
import Model

-- Drawing the world

drawWorld :: World -> Picture 
drawWorld world = 
    let 
        -- Retrieve the Player 
        p = player world

        -- Extract player coordinated
        (x, y) = position p

        -- Draw player as circle 
        -- view: draw the circle at the model's position
        playerPicture = translate x y (color green (circleSolid 20))
        mobPictures = map drawMob (mobs world)

     in pictures (playerPicture : mobPictures)

drawMob :: Mob -> Picture

drawMob mob =
    let
        (x, y) = mobPosition mob

    in translate x y (color red (circleSolid (mobRadius mob)))