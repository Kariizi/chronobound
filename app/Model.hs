-- Used to store any states or models used throughout the game
-- Game World
-- Players or Mobs, etc 

module Model where

-- Storing the game world:
data World = World 
    { player :: Player 
    , mobs :: [Mob]
    , heldKeys :: [Char]
    , projectiles :: [Projectile]
    } deriving (Show, Eq)

-- Storing information about the player

-- Declares type Player, where Player is the constructor
data Player = Player 
    -- All the fields
    { position :: (Float, Float) 
    , velocity :: (Float, Float)
    , health :: Int 
    , maxHealth :: Int
    , moveSpeed :: Float 
    , attackDamage :: Int
    , equippedWeapon :: Weapon
    } deriving (Show, Eq)

-- Storing information about the mobs
data Mob = Mob
    {mobPosition :: (Float, Float)
    , mobHealth :: Int
    , mobMaxHealth :: Int
    ,mobSpeed :: Float
    , mobDamage :: Int
    , mobRadius :: Float
    , mobAttackRange :: Float
    , mobZone :: Zone
    }
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
        , mobAttackRange = 30.0
        , mobZone = TopRight
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

-- Window size: change these and everything else follows
windowWidth, windowHeight :: Float
windowWidth  = 800
windowHeight = 600

worldHalfWidth, worldHalfHeight :: Float
worldHalfWidth  = windowWidth / 2
worldHalfHeight = windowHeight / 2

-- How far mobs spawn from the window edge and from the dividing lines
edgeMargin, lineMargin :: Float
edgeMargin = 30
lineMargin = 30

data Zone = TopLeft | TopRight | BottomLeft | BottomRight 
   deriving (Show, Eq, Enum, Bounded)

zoneOf :: (Float, Float) -> Zone
zoneOf (x, y)
    | x <  0 && y >= 0 = TopLeft
    | x >= 0 && y >= 0 = TopRight
    | x <  0 && y <  0 = BottomLeft
    | otherwise        = BottomRight

-- Declaring Upgrade Types:
data Upgrade 
    = HealthUpgrade Int 
    | SpeedUpgrade  Float
    | DamageUpgrade Int
    deriving (Show, Eq)

-- Defining Initial Attributes:

-- Initial players stats
initialPlayer :: Player 
initialPlayer = 
    Player
        { position = (-200, 150)
        , velocity = (0,0)
        , health = 100
        , maxHealth = 300 
        , moveSpeed = 200.0
        , attackDamage = 3
        , equippedWeapon = Weapon
    { weaponName = "Flintlock Pistol"
    , weaponDamage = 5
    , projectileSpeed = 6.0
    , attackRange = 300.0
    }
        }
        

-- Inital Game World
initialWorld :: World 
initialWorld = 
    World
        { player = initialPlayer
        , heldKeys = []
        , mobs = [initialMob]
        , projectiles = []
        }

