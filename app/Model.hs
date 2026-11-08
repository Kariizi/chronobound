
-- Used to store any states or models used throughout the game
-- Game World
-- Players or Mobs, etc

module Model where
import Data.List (sortOn)

import System.Random (StdGen, mkStdGen)

-- Storing the game world:
data World = World
    { player :: Player
    , mobs :: [Mob]
    , heldKeys :: [Char]
    , projectiles :: [Projectile]
    , damageNumbers :: [DamageNumber]
    , playerHistory :: [PlayerSnapshot]
    , worldTime :: Float
    , currentLevel :: Level
    -- For hell level:
    , hellTimeRemaining :: Float
    , hellKills :: Int 
    , gameState :: GameState
    , hellLayer :: Int 
    -- Storing Upgrades
    , upgradeChoices :: [Upgrade]
    -- Bonus Hell Time
    , bonusHellTime :: Float
    -- For generating Mobs
    , rng :: StdGen
    -- Rewind effect: time left on the effect, and where the player rewound from
    , rewindEffectTimer :: Float
    , rewindFrom :: (Float, Float)
    -- Victoria wave get harder based on waves cleared and time
    , victoriaWave :: Int
    , victoriaTime :: Float
    } deriving (Show, Eq)

-- Storing information about the player

-- Declares type Player, where Player is the constructor
data Player = Player
    -- All the fields
    { position :: (Float, Float)
    , velocity :: (Float, Float)
    , facingDirection :: (Float, Float)
    , health :: Int
    , maxHealth :: Int
    , moveSpeed :: Float
    -- Attack: how long attack remains, cooldown for attack
    , attackDamage :: Int
    , attackTimer :: Float
    , cooldownRemaining :: Float
    -- Dashing: how long dash remains, direction and cooldown
    , dashTimer :: Float
    , dashCooldownRemaining :: Float
    , dashDirection :: (Float, Float)
    -- Rewind:
    , rewindCooldownRemaining :: Float
    , equippedWeapon :: Weapon
    -- Time Health:
    , timeDrainAccumulator :: Float
    } deriving (Show, Eq)

data PlayerSnapshot = PlayerSnapshot
    -- Players Position
    { snapshotPosition :: (Float, Float)
    -- Players Health then
    , snapshotHealth :: Int
    -- When snapshot was recorded
    , snapshotAge :: Float
    } deriving (Show, Eq)

-- FOR MOB:

-- Storing Attacks of the mobs:
-- Chasing = follows play, winding up = preparing attack, Lunging = attacking
data MobAction
    = Chasing 
    | WindingUp Float 
    | Lunging Float (Float, Float)
    deriving (Show, Eq)

-- Storing information about the mobs
data Mob = Mob
    { mobPosition :: (Float, Float)
    , mobHealth :: Int
    , mobMaxHealth :: Int
    , mobSpeed :: Float
    , mobDamage :: Int
    , mobRadius :: Float
    , mobAttackRange :: Float
    , mobZone :: Zone
    , mobSprite :: String
    , mobAction :: MobAction

    -- Random animation timing for each mob
    , mobAnimOffset :: Float

    -- Time until mob can attack again
    , mobAttackCooldown :: Float

    -- Seconds between mob attacks
    , mobAttackInterval :: Float
    } deriving (Show, Eq)

-- Different dimensions
data Level 
    = VictoriaLevel
    | LimboLevel
    | HellLevel
    deriving (Show, Eq)

-- Stores gameState:
data GameState
    = Menu
    | Paused
    | Playing
    | ChoosingUpgrade
    | GameOver
    | Victory
    deriving (Show, Eq)

initialMob :: Mob
initialMob =
    Mob
        { mobPosition = (200,150)
        , mobHealth = 20
        , mobMaxHealth = 20
        , mobSpeed = 80.0
        , mobDamage = 5
        , mobRadius = 15.0
        , mobAttackRange = 5.0
        , mobZone = TopRight
        , mobSprite = "skeleton"

        -- Animation starts with no offset
        , mobAnimOffset = 0

        -- Mob can attack immediately
        , mobAttackCooldown = 0

        -- One second between attacks
        , mobAttackInterval = 1.0

        , mobAction = Chasing
        }

-- Storing information about weapons
data Weapon = Weapon
    { weaponName :: String
    , weaponDamage :: Int
    , projectileSpeed :: Float
    , attackRange :: Float
    } deriving (Show, Eq)

data Projectile = Projectile
    { projectilePosition :: (Float, Float)
    , projectileVelocity :: (Float, Float)
    , projectileDamage :: Int
    , projectileRange :: Float
    } deriving (Show, Eq)

-- Floating damage numbers
data DamageNumber = DamageNumber
    { damagePosition :: (Float, Float)
    , damageAmount :: Int
    , damageLifetime :: Float
    , damageHitsPlayer :: Bool
    -- True for time gained (shown as green +N)
    , gainedHP :: Bool
    } deriving (Show, Eq)

-- Window size: change these and everything else follows
windowWidth, windowHeight :: Float
windowWidth = 800
windowHeight = 600

worldHalfWidth, worldHalfHeight :: Float
worldHalfWidth = windowWidth / 2
worldHalfHeight = windowHeight / 2

-- How far mobs spawn from the window edge and from the dividing lines
edgeMargin, lineMargin :: Float
edgeMargin = 30
lineMargin = 30

playerRadius :: Float
playerRadius = 20

-- Can something stand at this position?
type Walkable = (Float, Float) -> Bool

-- Try the full move, then slide along x, then along y, otherwise stay put
-- Try the full move. If blocked, slide along a wall (the map's diagonal walls,
-- or straight x / y). If nothing works, get as close to the wall as possible.
resolveMove :: Walkable -> (Float, Float) -> (Float, Float) -> (Float, Float)
resolveMove ok (ox, oy) (nx, ny)
    | ok (nx, ny) = (nx, ny)
    | otherwise   = case filter ok slides of
        (p : _) -> p
        []      -> closest 0 1 (8 :: Int)
  where
    dx = nx - ox
    dy = ny - oy

    -- Wall directions: the diagonals of the tiles (2 across, 1 up) and x / y
    wallDirs = [(2, 1), (2, -1), (1, 0), (0, 1)]

    -- The part of the move that runs along a wall
    slide (wx, wy) =
        let n        = sqrt (wx * wx + wy * wy)
            (ux, uy) = (wx / n, wy / n)
            d        = dx * ux + dy * uy
        in (ox + ux * d, oy + uy * d)

    -- Try the slide that keeps the most movement first
    moved (x, y) = (x - ox) ^ 2 + (y - oy) ^ 2
    slides = sortOn (negate . moved) [ s | s <- map slide wallDirs, moved s > 0.0001 ]

    -- Move part of the way, as far as is still walkable
    closest lo hi 0 = lerp lo
    closest lo hi k =
        let mid = (lo + hi) / 2
        in if ok (lerp mid) then closest mid hi (k - 1) else closest lo mid (k - 1)
    lerp t = (ox + dx * t, oy + dy * t)

    
otherZoneSpeedFactor :: Float
otherZoneSpeedFactor = 0.25


data Zone = TopLeft | TopRight | BottomLeft | BottomRight
    deriving (Show, Eq, Enum, Bounded)

zoneOf :: (Float, Float) -> Zone
zoneOf (x, y)
    | x < 0 && y >= 0 = TopLeft
    | x >= 0 && y >= 0 = TopRight
    | x < 0 && y < 0 = BottomLeft
    | otherwise = BottomRight

-- Declaring Upgrade Types:
data Upgrade
    = HealthUpgrade Int
    | TimeUpgrade Float
    | SpeedUpgrade Float
    | DamageUpgrade Int
    deriving (Show, Eq)

-- Defining Initial Attributes:

-- Initial players stats
initialPlayer :: Player
initialPlayer =
    Player
        { position = (0, 0)
        , velocity = (0,0)
        , facingDirection = (1,0)
        , health = 100
        , maxHealth = 150
        , moveSpeed = 200.0
        , attackDamage = 4
        , attackTimer = 0
        , cooldownRemaining = 0
        , dashTimer = 0
        , dashCooldownRemaining = 0
        , dashDirection = (1, 0)
        , rewindCooldownRemaining = 0
        , timeDrainAccumulator = 0
        , equippedWeapon = Weapon
            { weaponName = "Flintlock Pistol"
            , weaponDamage = 5
            , projectileSpeed = 6.0
            , attackRange = 300.0
            }
        }

-- Initialising Variables: Functions can retrieve
dashSpeed :: Float
dashSpeed = 800

dashDuration :: Float
dashDuration = 0.25

dashCooldown :: Float
dashCooldown = 0.8

meleeAttackDistance :: Float
meleeAttackDistance = 40

meleeHitRadius :: Float
meleeHitRadius = 45

rewindCooldown :: Float
rewindCooldown = 5.0

-- Drop below this much time in Victoria and you fall into Limbo
limboEnterTime :: Int
limboEnterTime = 40

-- Get back above this much time in Limbo to escape to Victoria
-- (keep it well above limboEnterTime or the player bounces between the two)
limboExitTime :: Int
limboExitTime = 52

-- Time allowed to complete Deaths Trial
hellTrialDuration :: Float
hellTrialDuration = 60

-- The final layer of Hell contains the boss
finalHellLayer :: Int
finalHellLayer = 4

-- Maximum Time for Levels
displayMaxTime :: Float
displayMaxTime = 100

-- For Mob Attacks 

-- How long mobs prepare before lunging
mobWindupTime :: Float
mobWindupTime = 0.45

-- How long a mob spends lunging
mobLungeTime :: Float
mobLungeTime = 0.2

-- How quickly a mob lunges
mobLungeSpeed :: Float
mobLungeSpeed = 350

-- Number of enemies required to complete each layer
requiredKillsFor :: Int -> Int
requiredKillsFor layer
    | layer == 1 = 5
    | layer == 2 = 8
    | layer == 3 = 10
    | otherwise = 1

-- Number of kills required to escape Hell
hellRequiredKills :: Int
hellRequiredKills = 5

-- Inital Game World
initialWorld :: World
initialWorld =
    World
        { player = initialPlayer
        , heldKeys = []
        , mobs = [initialMob]
        , projectiles = []
        , damageNumbers = []
        , playerHistory = []
        , worldTime = 0
        , currentLevel = VictoriaLevel
        -- For hell
        , hellTimeRemaining = hellTrialDuration
        , hellKills = 0
        , hellLayer = 1
        , gameState = Playing
        , upgradeChoices = []
        , bonusHellTime = 0
        -- For generating Mobs
        , rng = mkStdGen 0
        -- No rewind effect playing at the start
        , rewindEffectTimer = 0
        , rewindFrom = (0, 0)
        -- Start on the first wave with no time spent in Victoria
        , victoriaWave = 1
        , victoriaTime = 0
        }

-- Every this many seconds spent in Victoria, time drains 1 second faster
drainIncreaseInterval :: Float
drainIncreaseInterval = 60

-- How many seconds of time the player loses each second
-- Starts at 1 and goes up the longer the player stays in Victoria
drainRateFor :: World -> Int
drainRateFor world =
    if currentLevel world == VictoriaLevel
        then 1 + floor (victoriaTime world / drainIncreaseInterval)
        else 1

-- How long the rewind flash, ring and streak last (seconds)
rewindEffectDuration :: Float
rewindEffectDuration = 0.4
