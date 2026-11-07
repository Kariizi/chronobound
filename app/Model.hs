-- Used to store any states or models used throughout the game
-- Game World
-- Players or Mobs, etc 

module Model where

-- Storing the game world:
data World = World 
    { player :: Player 
    } deriving (Show, Eq)

-- Storing information about the player
data Player = Player 
    { position :: (Float, Float) 
    , velocity :: (Float, Float)
    , health :: Int 
    } deriving (Show, Eq)


