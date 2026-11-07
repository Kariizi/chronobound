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
        mobPictures = map (drawMob sprites) (mobs world)

        -- Draw floating damage numbers
        damagePictures = map drawDamageNumber (damageNumbers world)

    -- Mobs at the back, damage numbers on top
    in pictures (mobPictures ++ [playerPicture, attackPicture] ++ damagePictures)


-- Draw individual mob
drawMob :: M.Map String Art -> Mob -> Picture
drawMob sprites mob =
    let (x, y) = mobPosition mob
        art    = sprites M.! mobSprite mob
        s      = 2
    in translate x y (scale s s (artPic art))

-- Draw a floating damage number
drawDamageNumber :: DamageNumber -> Picture
drawDamageNumber number =
    let
        -- Retrieve the damage number's position
        (x, y) = damagePosition number

        -- Convert damage amount from Int to String
        damageText = show (damageAmount number)

        textScale = 0.15

        numberColor = if damageHitsPlayer number
            then makeColor 1.0 0.2 0.2 1.0 
            else makeColor 1.0 1.0 0.2 1.0 

    in translate x y
        (scale textScale textScale
            (color numberColor (Text ("-" ++ damageText))))