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
        playerPicture = translate x y (color green (circleSolid playerRadius))
        mobPictures = map drawMob (mobs world)
        dividers = color (greyN 0.7) $ pictures
            [ line [(-worldHalfWidth, 0), (worldHalfWidth, 0)]
            , line [(0, -worldHalfHeight), (0, worldHalfHeight)]
            ]


     in pictures (dividers : playerPicture : mobPictures)

drawMob :: Mob -> Picture

drawMob mob =
    let
        (x, y) = mobPosition mob
        zoneColour = case mobZone mob of
            TopLeft     -> red
            TopRight    -> orange
            BottomLeft  -> magenta
            BottomRight -> blue

    in translate x y (color zoneColour (circleSolid (mobRadius mob)))