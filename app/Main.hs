module Main where

import Graphics.Gloss.Interface.Pure.Game
import Model
import View
import Controller

main :: IO ()
main = play
    (InWindow "Chronobound" (800, 600) (100, 100))
    white
    60
    initialWorld
    drawWorld
    handleInput
    updateWorld