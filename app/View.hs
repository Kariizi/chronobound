
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
    in pictures
        (mobPictures ++ [drawRewindTrail world, drawRewindPreview world, playerPicture] ++ damagePictures)


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
        -- Draw players remaining time as a clock
        timeHUD = drawTimeClock world

        -- Extra Deaths Trial panel, only shown in Hell
        hellHUD =
            if currentLevel world == HellLevel
                then drawHellHUD world
                else Blank

    in pictures [timeHUD, hellHUD]


-- Draw the player's remaining time as a clock
drawTimeClock :: World -> Picture
drawTimeClock world =
    let
        -- Retrieve player and remaining time
        p = player world
        currentHP = health p

        -- Calculate how much of the clock remains
        timeFraction = max 0 (min 1 (fromIntegral currentHP / displayMaxTime))

        -- Clock dimensions
        clockRadius = 34
        ringThickness = 4

        -- Colour of clock depending on remaining time
        clockColour
            | currentLevel world == LimboLevel || currentHP < 30 =
                makeColor 0.9 0.2 0.2 1

            | currentHP <= 40 =
                makeColor 0.65 0.4 0.9 1

            | otherwise =
                makeColor 0.95 0.75 0.35 1

        -- Dark background for the clock
        clockBackground =
            color (makeColor 0.12 0.1 0.14 0.9)
                (circleSolid (clockRadius + 5))

        -- Outline of the full clock
        clockOutline =
            color (makeColor 0.3 0.25 0.3 1)
                (thickCircle clockRadius ringThickness)

        -- Calculate how much of the circular ring is filled
        filledAngle = 360 * timeFraction

        -- Draw remaining time as a circular arc, filling clockwise from 12 o'clock
        -- (drawn from 0 then rotated, because Gloss swaps arc angles that wrap past 0)
        clockFill
            | timeFraction <= 0 = Blank
            | otherwise =
                rotate (filledAngle - 90)
                    (color clockColour
                        (thickArc 0 filledAngle clockRadius ringThickness))

        -- Draw small clock markings
        clockTicks =
            pictures
                [ rotate (fromIntegral i * 30)
                    (color (makeColor 0.7 0.6 0.5 0.8)
                        (line [(0, clockRadius - 10), (0, clockRadius - 5)]))
                | i <- [0 .. 11]
                ]

        -- Display remaining time inside the clock
        -- Number and label are stacked so the pair sits centred on the clock face
        timeNumber =
            translate 0 7
                (centredText 0.19 white (show currentHP))

        timeLabel =
            translate 0 (-13)
                (centredText 0.08 clockColour "TIME")

        -- Position clock near top-left
        hudX = -worldHalfWidth + 65
        hudY = worldHalfHeight - 65

    in translate hudX hudY
        (pictures
            [ clockBackground
            , clockOutline
            , clockFill
            , clockTicks
            , timeNumber
            , timeLabel
            ])


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
        -- Real width of the text (letters have different widths), and roughly 100 tall, before scaling
        textWidth = textWidthUnits str * size
        textHeight = 100 * size

    in translate (-textWidth / 2) (-textHeight / 2)
        (scale size size (color colour (Text str)))


-- Width of a string in Gloss text units (before scaling)
textWidthUnits :: String -> Float
textWidthUnits = sum . map glyphWidth

-- Width of one character in Gloss's built-in font (GLUT Roman), measured with GLUT's stringWidth
-- Characters not in the table fall back to 100
glyphWidth :: Char -> Float
glyphWidth c =
    case lookup c glyphWidths of
        Just w  -> w
        Nothing -> 100

-- Widths for every printable character from ' ' to '~', in order
glyphWidths :: [(Char, Float)]
glyphWidths = zip [' ' .. '~']
    [ 104, 26, 51, 79, 76, 96, 101, 13, 47, 47, 59, 97, 26, 100, 26, 82   -- space ! " # $ % & ' ( ) * + , - . /
    , 77, 66, 77, 77, 80, 77, 73, 77, 77, 74                              -- 0 to 9
    , 26, 26, 81, 97, 81, 73, 74                                          -- : ; < = > ? @
    , 80, 83, 84, 85, 78, 78, 89, 89, 21, 59, 79, 71, 97                  -- A to M
    , 88, 88, 85, 88, 82, 80, 71, 89, 81, 100, 72, 79, 73                 -- N to Z
    , 46, 78, 46, 90, 104, 83                                             -- [ \ ] ^ _ `
    , 66, 70, 68, 70, 68, 38, 70, 71, 28, 36, 62, 19, 123                 -- a to m
    , 70, 71, 70, 70, 49, 62, 39, 71, 60, 80, 56, 66, 61                  -- n to z
    , 41, 23, 41, 91                                                      -- { | } ~
    ]


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

-- Draw pause menu when the game is paused
drawPauseScreen :: World -> Picture
drawPauseScreen world
    | gameState world == Paused =
        let
            gold = makeColor 1 0.8 0.3 1
            pale = makeColor 0.8 0.78 0.85 1

            -- Slowly pulse the resume instruction
            pulse = (sin (worldTime world * 3) + 1) / 2
            resumeColour = makeColor 1 1 1 (0.4 + 0.6 * pulse)

            -- Where the player is, shown under the title
            whereText =
                case currentLevel world of
                    VictoriaLevel -> "Victorian London"
                    LimboLevel    -> "Limbo"
                    HellLevel     -> "Hell - Layer " ++ show (hellLayer world)

        in pictures
            [ -- Dark overlay, slightly lighter than the menu so the frozen game shows through
              color (makeColor 0.02 0.01 0.03 0.8)
                  (rectangleSolid windowWidth windowHeight)

              -- Title shadow
            , translate 3 127
                (centredText 0.4 (makeColor 0.2 0.1 0.05 1) "PAUSED")

              -- Title
            , translate 0 130
                (centredText 0.4 gold "PAUSED")

              -- Current dimension
            , translate 0 80
                (centredText 0.11 pale whereText)

              -- Controls
            , controlLine 30 "WASD" "Move"
            , controlLine 0 "SPACE" "Attack"
            , controlLine (-30) "F" "Dash"
            , controlLine (-60) "R" "Rewind"
            , controlLine (-90) "ESC" "Resume"

              -- Resume instruction
            , translate 0 (-160)
                (centredText 0.15 resumeColour "Press ESC to resume")
            ]
    | otherwise = Blank


-- One control line: key in gold on the left, action in white on the right
-- (shared by the start screen and the pause screen)
controlLine :: Float -> String -> String -> Picture
controlLine y key action =
    pictures
        [ translate (-70) y (centredText 0.11 (makeColor 1 0.8 0.3 1) key)
        , translate 70 y (centredText 0.11 white action)
        ]


-- Ending screen after defeating the boss, revealing the story's twist
drawVictory :: World -> Picture
drawVictory world
    | gameState world == Victory =
        let
            -- Total run time as minutes:seconds
            totalSeconds = floor (worldTime world) :: Int
            (minutes, seconds) = totalSeconds `divMod` 60
            padded n = if n < 10 then '0' : show n else show n
            timeText = "Time survived: " ++ show minutes ++ ":" ++ padded seconds

            gold = makeColor 1 0.8 0.3 1
            pale = makeColor 0.85 0.85 0.9 1

        in pictures
            [ -- Darker overlay than Game Over so the story text stands out
              color (makeColor 0 0 0 0.8) (rectangleSolid windowWidth windowHeight)

              -- Title
            , translate 0 110 (centredText 0.35 gold "YOU SURVIVED")

              -- The README twist
            , translate 0 40 (centredText 0.13 pale "The monsters you killed... were human.")
            , translate 0 10 (centredText 0.13 pale "The Clock has chosen you as its servant.")

              -- Run stats
            , translate 0 (-45) (centredText 0.12 white timeText)

              -- Restart hint
            , translate 0 (-110) (centredText 0.15 white "Press ENTER to play again")
            ]
    | otherwise = Blank

-- Title screen shown when the game first opens
drawStartScreen :: World -> Picture
drawStartScreen world
    | gameState world == Menu =
        let
            gold = makeColor 1 0.8 0.3 1
            pale = makeColor 0.8 0.78 0.85 1

            -- Slowly pulse the start instruction
            pulse = (sin (worldTime world * 3) + 1) / 2
            startColour = makeColor 1 1 1 (0.4 + 0.6 * pulse)

        in pictures
            [ -- Almost opaque so the title stands out over the paused map
              color (makeColor 0.02 0.01 0.03 0.88)
                  (rectangleSolid windowWidth windowHeight)

              -- Title shadow
            , translate 3 187
                (centredText 0.45 (makeColor 0.2 0.1 0.05 1) "CHRONOBOUND")

              -- Title
            , translate 0 190
                (centredText 0.45 gold "CHRONOBOUND")

              -- Premise
            , translate 0 125
                (centredText 0.11 pale "Your time is your life.")
            , translate 0 100
                (centredText 0.11 pale "Kill to steal more... before it runs out.")

              -- Controls
            , controlLine 40 "WASD" "Move"
            , controlLine 10 "SPACE" "Attack"
            , controlLine (-20) "F" "Dash"
            , controlLine (-50) "R" "Rewind"
            , controlLine (-80) "ESC" "Pause"

              -- Start prompt
            , translate 0 (-170)
                (centredText 0.16 startColour "Press ENTER to begin")
            ]
    | otherwise = Blank


-- Upgrade selection screen shown between Hell layers
drawUpgradeScreen :: World -> Picture
drawUpgradeScreen world
    | gameState world == ChoosingUpgrade =
        let
            choices = upgradeChoices world
            count = length choices

            -- Space between card centres
            cardSpacing = 240

            -- Centre the row of cards whatever the number of choices
            cardX index = (fromIntegral index - (fromIntegral count + 1) / 2) * cardSpacing

            -- Pair each upgrade with its key number (1, 2, 3)
            cards =
                [ translate (cardX index) (-10) (drawUpgradeCard index upgrade)
                | (index, upgrade) <- zip [1 ..] choices
                ]

            ivory = makeColor 0.95 0.9 0.75 1

        in pictures (
            [ -- Dim the game behind the screen
              color (makeColor 0 0 0 0.75) (rectangleSolid windowWidth windowHeight)

              -- Title
            , translate 0 190 (centredText 0.3 ivory "CHOOSE AN UPGRADE")
            , translate 0 150 (centredText 0.11 (makeColor 0.7 0.65 0.6 1) "Death grants you one blessing...")
            ]
            ++ cards ++
            [ -- How to choose
              translate 0 (-170) (centredText 0.13 white "PRESS 1, 2 OR 3 TO CHOOSE")
            ])
    | otherwise = Blank

-- One upgrade card: key number, name and effect, coloured by upgrade type
drawUpgradeCard :: Int -> Upgrade -> Picture
drawUpgradeCard index upgrade =
    let
        cardWidth = 200
        cardHeight = 180

        accent = upgradeColour upgrade

        -- Darker version of the accent for the card background
        background = color (makeColor 0.08 0.06 0.08 0.95) (rectangleSolid cardWidth cardHeight)

        -- Double border in the accent colour
        border = color accent (pictures
            [ rectangleWire cardWidth cardHeight
            , rectangleWire (cardWidth - 8) (cardHeight - 8)
            ])

        keyLabel = translate 0 50 (centredText 0.22 accent ("[" ++ show index ++ "]"))
        title = translate 0 0 (centredText 0.11 white (upgradeTitle upgrade))
        description = translate 0 (-40) (centredText 0.09 accent (upgradeDescription upgrade))

    in pictures [background, border, keyLabel, title, description]

-- Name shown on each upgrade card
upgradeTitle :: Upgrade -> String
upgradeTitle upgrade = case upgrade of
    TimeUpgrade _   -> "Borrowed Time"
    DamageUpgrade _ -> "Sharpened Blade"
    SpeedUpgrade _  -> "Fleeting Step"
    HealthUpgrade _ -> "Stolen Years"

-- Short effect text (kept short because Gloss text does not wrap)
upgradeDescription :: Upgrade -> String
upgradeDescription upgrade = case upgrade of
    TimeUpgrade amount   -> "+" ++ show (round amount :: Int) ++ " seconds"
    DamageUpgrade amount -> "+" ++ show amount ++ " damage"
    SpeedUpgrade amount  -> "+" ++ show (round amount :: Int) ++ " movement speed"
    HealthUpgrade amount -> "+" ++ show amount ++ " max time"

-- Accent colour for each upgrade type
upgradeColour :: Upgrade -> Color
upgradeColour upgrade = case upgrade of
    TimeUpgrade _   -> makeColor 1 0.75 0.3 1      -- gold
    DamageUpgrade _ -> makeColor 0.85 0.25 0.25 1  -- crimson
    SpeedUpgrade _  -> makeColor 0.45 0.8 0.85 1   -- pale teal
    HealthUpgrade _ -> makeColor 0.5 0.85 0.5 1    -- green

-- Draws a marker showing where the player will rewind to
drawRewindPreview :: World -> Picture
drawRewindPreview world =
    let
        p = player world
        history = playerHistory world
    in
        if rewindCooldownRemaining p > 0 || null history
            then Blank
        else
            let
                -- Retrieve the oldest saved player position
                (x, y) = snapshotPosition (last history)
                -- Create the rewind marker
                marker = pictures
                    [ color (makeColor 0.4 0.85 1 1) (thickCircle 22 3)
                    , color white (line [(0, 0), (0, 12)])
                    , color white (line [(0, 0), (9, 0)])
                    ]
            in translate x y marker

-- Draws a trail showing the player's previous positions
drawRewindTrail :: World -> Picture
drawRewindTrail world =
    let
        p = player world
        history = playerHistory world

        -- Keep every 6th snapshot so there aren't too many dots
        snapshots =
            [ snapshot
            | (index, snapshot) <- zip [0..] history
            , index `mod` 6 == 0
            ]

        -- Draw a dot at each saved position
        drawDot snapshot =
            let
                (x, y) = snapshotPosition snapshot

            in translate x y
                (color (makeColor 0.4 0.85 1 0.65)
                    (circleSolid 3))

    in
        if rewindCooldownRemaining p > 0 || null history
            then Blank
            else pictures (map drawDot snapshots)


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