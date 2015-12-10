module Shapes ( 
	Point(..), 
	Shape(..), 
	Size(..),
	surface,
	translate,
	basicCircle,
	basicRectangle
	)where


data Point = Point Float Float deriving (Show)
data Size  = Size Float Float deriving (Show)
data Shape = Circle Point Float | Rectangle Point Size deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle _ (Size w h)) = w * h

translate :: Shape -> Point -> Shape
translate (Circle (Point x y) r) (Point a b) = Circle (Point (x+a) (y+b)) r
translate (Rectangle (Point x y) (Size w h)) (Point a b) = Rectangle (Point (x+a) (y+b)) (Size w h)

basicCircle :: Float -> Shape
basicCircle r = Circle (Point 0 0) r

basicRectangle :: Size -> Shape
basicRectangle (Size w h) = Rectangle (Point 0 0) (Size w h)