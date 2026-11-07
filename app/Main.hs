import Graphics.Gloss.Interface.Pure.Game
import Model
import View
import Controller

-- Creating Model Type: where circle is
type Model = (Float, Float)

initialModel :: Model 
initialModel = (0,0)

-- view: draw the circle at the model's position
view :: Model -> Picture 
view (x, y) = translate x y (circle 20)

-- input: arrow keys to move the circle (change for sprites later)\
-- input: should prpbably use wasd too
handleInput :: Event -> Model -> Model 
handleInput (EventKey (SpecialKey KeyRight) Down _ _) (x, y) = (x + 20, y)
handleInput (EventKey (SpecialKey KeyLeft) Down _ _) (x, y) = (x - 20, y)
handleInput (EventKey (SpecialKey KeyUp) Down _ _) (x, y) = (x, y + 20)
handleInput (EventKey (SpecialKey KeyDown) Down _ _) (x, y) = (x, y - 20)

handleInput (EventKey (Char 'w') Down _ _) (x, y) = (x, y + 20)
handleInput (EventKey (Char 'a') Down _ _) (x, y) = (x - 20, y)
handleInput (EventKey (Char 's') Down _ _) (x, y) = (x, y - 20)
handleInput (EventKey (Char 'd') Down _ _) (x, y) = (x + 20, y)

handleInput _ model = model 

-- time: nothing changes over time yet
update :: Float -> Model -> Model
update _ model = model

main :: IO ()
main = play (InWindow "Chronobound" (800, 600) (100, 100)) white 60 initialModel view handleInput update