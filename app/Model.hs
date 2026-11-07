-- Used to store any states or models used throughout the game
-- Game World
-- Players or Mobs, etc 

module Model where

-- Storing the game world:
data World = World 
    { player :: Player 
    , heldKeys :: [Char]
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
        }

-- Inital Game World
initialWorld :: World 
initialWorld = 
    World
        { player = initialPlayer
        -- Initially no keys are pressed
        , heldKeys = []
        }

