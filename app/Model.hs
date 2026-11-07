-- Used to store any states or models used throughout the game
-- Game World
-- Players or Mobs, etc 

module Model where

-- Storing the game world:
data World = World 
    { player :: Player 
    , mobs :: [Mob]
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
    , mobMaxHealth :: Int}
    deriving (Show, Eq)
initialMob :: Mob
initialMob =
    Mob
        { mobPosition = (500, 300)
        , mobHealth = 20
        , mobMaxHealth = 20
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
        { position = (0, 0)
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
        , mobs = [initialMob]
        , projectiles = []
        }

