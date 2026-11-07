module View where

import Graphics.Gloss
import Model
import Combat (isAttacking)
import Player (isDashing)
import Model (meleeAttackDistance)

-- Drawing the world
drawWorld :: World -> Picture
drawWorld world =
    let
        -- Retrieve the player
        p = player world

        -- Player position
        (x, y) = position p

        -- Player facing direction
        (fx, fy) = facingDirection p

        -- Change colour when dashing
        playerColour =
            if isDashing p
                then blue
                else green

        -- Draw player
        playerPicture =
            translate x y (color playerColour (circleSolid 20))

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
        damagePictures = map drawDamageNumber (damageNumbers world)

    -- Combine player, attack and mobs
    in pictures (mobPictures ++ [playerPicture, attackPicture] ++ damagePictures)


-- Draw individual mob
drawMob :: Mob -> Picture
drawMob mob =
    let
        (x, y) = mobPosition mob

    in translate x y
        (color red (circleSolid (mobRadius mob)))

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