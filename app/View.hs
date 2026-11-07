module View where

import Graphics.Gloss
import Model
import Combat (isAttacking)
import qualified Data.Map.Strict as M
import MapArt

-- Drawing the world
drawWorld :: M.Map String Art -> World -> Picture
drawWorld sprites world =
    let
        -- Retrieve the player
        p = player world

        -- Player position
        (x, y) = position p

        -- Player facing direction
        (fx, fy) = facingDirection p

        -- Draw player, flipped to face left or right
        playerPicture =
            let art = sprites M.! "normal"
                s   = 2
                sx  = if fx < 0 then -s else s
            in translate x y (scale sx s (artPic art))

        -- How far in front of the player the attack appears
        attackDistance = meleeAttackDistance

        -- Calculate attack position
        attackX = x + fx * attackDistance
        attackY = y + fy * attackDistance

        -- Draw melee attack indicator
        attackPicture =
            if isAttacking p
                then translate attackX attackY
                    (color orange (circleSolid 15))
                else Blank

        -- Draw all mobs
        mobPictures = map drawMob (mobs world)

        -- Draw floating damage numbers
        damagePictures = map drawDamageNumber (damageNumbers world)

        -- Zone divider lines
        dividers = color (greyN 0.7) $ pictures
            [ line [(-worldHalfWidth, 0), (worldHalfWidth, 0)]
            , line [(0, -worldHalfHeight), (0, worldHalfHeight)]
            ]

    -- Dividers at the back, damage numbers on top
    in pictures ([dividers] ++ mobPictures ++ [playerPicture, attackPicture] ++ damagePictures)


-- Draw individual mob
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

-- Draw a floating damage number
drawDamageNumber :: DamageNumber -> Picture
drawDamageNumber number =
    let
        -- Retrieve the damage number's position
        (x, y) = damagePosition number

        -- Convert damage amount from Int to String
        damageText = show (damageAmount number)

        textScale = 0.15

    in translate x y
        (scale textScale textScale
            (color yellow (Text ("-" ++ damageText))))