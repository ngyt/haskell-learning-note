-- Testing the Shapes module created in haskell_Type.hs
import Shapes

scale :: Shape -> Float -> Shape
scale (Circle (Point x y) r) s = Circle (Point x y) (r*s)
scale (Rectangle (Point x y) (Size w h)) = Rectangle (Point x y) (Size (w*s) (h*s))