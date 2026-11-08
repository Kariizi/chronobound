
module View where

import Graphics.Gloss
import Model
import Combat (isAttacking)
import Player (isDashing)
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

        -- Draw player using the animation matching their current action
        playerPicture =
            let
                -- Determine which animation the player should use
                animationType
                    -- Highest priority: dashing
                    | isDashing p = "dash"

                    -- Attack animation plays throughout the attack cooldown
                    | cooldownRemaining p > 0 = "attack"

                    -- Walking when the player is moving
                    | velocity p /= (0, 0) = "walk"

                    -- Otherwise player is standing still
                    | otherwise = "breathe"

                -- Combine character name with animation type
                animationName = "player_" ++ animationType

                -- Retrieve the animation
                animation = sprites M.! animationName

                -- Calculate progress through the dash
                dashProgress = 1 - dashTimer p / dashDuration

                -- Calculate progress through the attack
                attackProgress = 1 - cooldownRemaining p / 0.4

                -- Choose whether the animation loops or plays once
                art
                    | animationType == "dash" =
                        frameAtProgress animation dashProgress

                    | animationType == "attack" =
                        frameAtProgress animation attackProgress

                    | otherwise =
                        frameAt animation (worldTime world)

                -- Keep existing sprite scaling and facing direction
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

        -- Green +N for time gained, red -N for player damage, yellow -N for mobs
        (numberColor, sign)
            | gainedHP number     = (makeColor 0.2 1.0 0.3 1.0, "+")
            | damageHitsPlayer number = (makeColor 1.0 0.2 0.2 1.0, "-")
            | otherwise               = (makeColor 1.0 1.0 0.2 1.0, "-")

    in translate x y
        (scale textScale textScale
            (color numberColor (Text (sign ++ damageText))))


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

-- Select a frame based on animation progress (0 = start, 1 = end)
frameAtProgress :: Animation -> Float -> Art
frameAtProgress anim progress =
    let
        -- Gets total duration of the animation
        totalDuration = sum (animDelays anim)

        -- Keep progress between 0 and 1
        safeProgress = max 0 (min 1 progress)

        -- Convert progress into animation time = gets the frame we are at
        -- Multiplying percentage of frame we are at by total duration
        animationTime = safeProgress * totalDuration

    -- Show the last frame when finished, otherwise select the current frame
    in if safeProgress >= 1
        then last (animFrames anim)
        else frameAt anim animationTime