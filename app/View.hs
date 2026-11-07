
module View where

import Graphics.Gloss
import Model
import Combat (isAttacking)
import qualified Data.Map.Strict as M
import MapArt
import Data.Fixed (mod')

-- Drawing the world
drawWorld :: M.Map String Animation -> World -> Picture
drawWorld sprites world =
    let
        -- Retrieve the player
        p = player world

        -- Player position
        (x, y) = position p

        -- Player facing direction
        (fx, fy) = facingDirection p

        -- Draw player using breathing animation, flipped to face left or right
        playerPicture =
            let
                -- Retrieves current animation frame based on game time
                art = frameAt (sprites M.! "player_breathe") (worldTime world)

                s = 2
                sx = if fx < 0 then -s else s

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
        mobPictures = map (drawMob sprites (worldTime world)) (mobs world)

        -- Draw floating damage numbers
        damagePictures = map drawDamageNumber (damageNumbers world)

    -- Mobs at the back, damage numbers on top
    in pictures (mobPictures ++ [playerPicture, attackPicture] ++ damagePictures)


-- Draw individual mob using its animation
drawMob :: M.Map String Animation -> Float -> Mob -> Picture
drawMob sprites time mob =
    let
        -- Retrieve mob position
        (x, y) = mobPosition mob

        -- Select animation frame with individual mob offset
        art = frameAt (sprites M.! mobSprite mob)
            (time + mobAnimOffset mob)

        s = 2

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

        -- Red damage numbers for player, yellow for mobs
        numberColor =
            if damageHitsPlayer number
                then makeColor 1.0 0.2 0.2 1.0
                else makeColor 1.0 1.0 0.2 1.0

    in translate x y
        (scale textScale textScale
            (color numberColor (Text ("-" ++ damageText))))


-- Drawing HUD for HP
drawHUD :: World -> Picture
drawHUD world =
    let
        -- Retrieve player and health
        p = player world
        currentHP = health p
        maximumHP = maxHealth p

        -- Convert HP into text
        hpText = "TIME: " ++ show currentHP

        -- Position HUD near top-left of screen
        hudX = -worldHalfWidth + 20
        hudY = worldHalfHeight - 40

    in translate hudX hudY
        (scale 0.15 0.15
            (color white (Text hpText)))


-- Which frame of an animation to show at time t (loops forever)
frameAt :: Animation -> Float -> Art
frameAt anim t = go (animFrames anim) (animDelays anim) (t `mod'` total)
    where
        -- Total duration of all animation frames
        total = sum (animDelays anim)

        -- Takes current frame, delay and remaining time
        go (f : fs) (d : ds) remaining
            -- Return frame if time is within its duration or it's the last frame
            | remaining < d || null fs = f
            -- Otherwise skip frame and subtract its duration
            | otherwise = go fs ds (remaining - d)

        -- Error if no frames are available
        go _ _ _ = error "animation with no frames"
