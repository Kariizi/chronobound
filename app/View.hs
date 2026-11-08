
module View where

import Graphics.Gloss
import Model
import Player (isDashing)
import Mob (distanceToPlayer)
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
        (fx, _) = facingDirection p

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

                -- Player looks corrupted once they fall into Limbo or Hell
                characterName = case currentLevel world of
                    VictoriaLevel -> "player"
                    LimboLevel    -> "player_corrupted"
                    HellLevel     -> "player_corrupted"

                -- Combine character name with animation type
                animationName = characterName ++ "_" ++ animationType

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

        -- Draw all mobs
        mobPictures = map (drawMob sprites (worldTime world) p) (mobs world)

        -- Draw floating damage numbers
        damagePictures = map drawDamageNumber (damageNumbers world)

    -- Mobs at the back, damage numbers on top
    in pictures (mobPictures ++ [playerPicture] ++ damagePictures)


-- How long a mob's attack animation plays after it hits (length of the attack GIF)
mobAttackAnimDuration :: Float
mobAttackAnimDuration = 0.65

-- Draw individual mob using the animation matching its current action
drawMob :: M.Map String Animation -> Float -> Player -> Mob -> Picture
drawMob sprites time p mob =
    let
        -- Retrieve mob and player positions
        (x, y) = mobPosition mob
        (px, _) = position p

        -- Time since the mob last hit (cooldown resets to the interval on a hit)
        timeSinceHit = mobAttackInterval mob - mobAttackCooldown mob

        -- Mob chases when the player is in its zone (same rule as moveMobs)
        isChasing = mobZone mob == zoneOf (position p)

        -- Close enough to hit, so it stands still instead of walking in place
        inReach = distanceToPlayer p mob
            <= playerRadius + mobRadius mob + mobAttackRange mob

        -- Determine which animation the mob should use
        animationType
            -- Highest priority: just hit the player
            | mobAttackCooldown mob > 0 && timeSinceHit < mobAttackAnimDuration = "attack"

            -- Moving towards the player
            | isChasing && not inReach = "walk"

            -- Otherwise idle
            | otherwise = "breathe"

        -- Combine character name with animation type
        animation = sprites M.! (mobSprite mob ++ "_" ++ animationType)

        -- Progress through the attack, 0 at the hit and 1 when the GIF finishes
        attackProgress = timeSinceHit / mobAttackAnimDuration

        -- Attack plays once, walk and breathe loop (with the mob's own offset)
        art
            | animationType == "attack" = frameAtProgress animation attackProgress
            | otherwise                 = frameAt animation (time + mobAnimOffset mob)

        -- Flip the sprite to face the player
        s = 2
        sx = if px < x then -s else s

    in translate x y (scale sx s (artPic art))


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

        timeHUD = translate hudX hudY
            (scale 0.15 0.15
                (color white (Text hpText)))

        -- Extra Death's Trial panel, only shown in Hell
        hellHUD =
            if currentLevel world == HellLevel
                then drawHellHUD world
                else Blank

    in pictures [timeHUD, hellHUD]


-- Death's Trial HUD: countdown bar, seconds left and kills needed to escape
drawHellHUD :: World -> Picture
drawHellHUD world =
    let
        timeLeft = hellTimeRemaining world
        kills = hellKills world

        currentLayer = hellLayer world
        requiredKills = requiredKillsFor currentLayer

        -- Fraction of the trial time remaining (0 to 1)
        timeFraction = max 0 (min 1 (timeLeft / hellTrialDuration))

        -- Panel sits at the top centre of the screen
        panelY = worldHalfHeight - 35

        -- Countdown bar size
        barWidth = 240
        barHeight = 10

        -- Pulse between dark and bright red when under 10 seconds
        pulse = (sin (worldTime world * 10) + 1) / 2
        barColour =
            if timeLeft < 10
                then makeColor (0.5 + 0.5 * pulse) 0.05 0.05 1
                else makeColor 0.85 0.1 0.1 1

        -- Title
        titleText =
            if currentLayer == finalHellLayer
                then "THE CLOCKMAKER"
                else "DEATH'S TRIAL - LAYER " ++ show currentLayer

        title = centredText 0.18 (makeColor 1 0.25 0.15 1) titleText

        -- Dark background bar, with the red fill shrinking towards the left
        barBackground = color (makeColor 0.2 0 0 0.8)
            (rectangleSolid barWidth barHeight)

        fillWidth = barWidth * timeFraction
        barFill = translate (-(barWidth - fillWidth) / 2) 0
            (color barColour (rectangleSolid fillWidth barHeight))

        -- Seconds left and kill progress underneath the bar
        infoText = "KILLS " ++ show kills ++ " / " ++ show requiredKills
            ++ "    " ++ show (ceiling timeLeft :: Int) ++ "s"
        info = centredText 0.12 white infoText

    in translate 0 panelY (pictures
        [ title
        , translate 0 (-22) (pictures [barBackground, barFill])
        , translate 0 (-45) info
        ])


-- Draw text roughly centred on the origin (Gloss draws text from its bottom-left corner)
centredText :: Float -> Color -> String -> Picture
centredText size colour str =
    let
        -- Each Gloss character is roughly 100 units wide and 100 tall before scaling
        textWidth = fromIntegral (length str) * 100 * size
        textHeight = 100 * size

    in translate (-textWidth / 2) (-textHeight / 2)
        (scale size size (color colour (Text str)))


-- Dark overlay with the Game Over message, drawn on top of everything
drawGameOver :: World -> Picture
drawGameOver world
    | gameState world == GameOver =
        pictures
            [ -- Semi-transparent black over the whole window
              color (makeColor 0 0 0 0.7) (rectangleSolid windowWidth windowHeight)

              -- Large title, slightly above centre
            , translate 0 30 (centredText 0.4 (makeColor 0.9 0.15 0.1 1) "TIME'S UP")

              -- Restart hint underneath
            , translate 0 (-40) (centredText 0.15 white "Press ENTER to restart")
            ]
    | otherwise = Blank


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