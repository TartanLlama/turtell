module Turtle where

import Graphics.Gloss
import Parsing

data Turtle = Turtle { location :: Point,
                       direction :: Float,
                       pen_colour :: Color,
                       pen_down :: Bool }

rad :: Float -> Float
rad r = r*(pi/180)

start = Turtle (0,0) 0 white True

type Command = Turtle -> (Turtle, Picture)

fd :: Float -> Command
fd dist t = do let (x, y) = location t 
               let  x' = x + dist * sin (rad (direction t))
               let  y' = y + dist * cos (rad (direction t))
               if pen_down t
                  then
                      (t { location = (x',y') }, 
                         Color (pen_colour t) (Line [(x,y), (x',y')]))
                  else
                      (t { location = (x',y') }, Blank)
                             
rt :: Float -> Command
rt angle t = (t { direction = bound (angle + direction t) }, Blank)
   where bound ang | ang > 360 = bound (ang - 360)
                   | ang < 0 = bound (ang + 360)
                   | otherwise = ang

chColour :: Color -> Command
chColour c t = (t {pen_colour = c}, Blank)

penUp :: Command
penUp t = (t {pen_down = False}, Blank)

penDown :: Command
penDown t = (t {pen_down = True}, Blank)

mv :: (Float, Float) -> Command
mv (x, y) t = do let (x', y') = location t
                 if pen_down t 
                     then (t {location = (x, y)}, Color (pen_colour t) (Line [(x',y'), (x,y)]))
                     else (t {location = (x, y)}, Blank)
                    

nothing :: Command
nothing t = (t, Blank)

-- +> operator used to chain commands
(+>) :: Command -> Command -> Command
(+>) c1 c2 t = let (t', p1) = c1 t
                   (t'', p2) = c2 t' in
                   (t'', Pictures [p1, p2])

--Example command:
octagon :: Float -> Command
octagon x = rt 45 +> fd x +> rt 45 +> fd x +>
            rt 45 +> fd x +> rt 45 +> fd x +>
            rt 45 +> fd x +> rt 45 +> fd x +>
            rt 45 +> fd x +> rt 45 +> fd x

--Called from main with compiled command from runMain/interp
runTurtle :: Maybe Command -> IO ()
runTurtle cmd = 
    case cmd of
      Just c -> displayInWindow "Turtell" (640,480) (50,50) black 
                (snd (c start))
      Nothing -> putStrLn "Type error."




