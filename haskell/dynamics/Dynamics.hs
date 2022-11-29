{-# OPTIONS -Wall -Werror #-}

module Dynamics
(
 Vector(Vector),
 vector_length,
 vector_angle,
 Coordinates,
 coordinates,
 Plane(Plane),
 plane,
 plane_angle,
 vector_plane_angle,
 plane_vector_angle,
) where

data Vector = Vector {x :: Float, y :: Float, z :: Float}

type Coordinates = Vector

coordinates :: Float -> Float -> Float -> Coordinates
coordinates = Vector

infixl 7 .*
(.*) :: Vector -> Vector -> Float
v .* w = x v * x w + y v * y w + z v * z w

vector_length :: Vector -> Float
vector_length v = sqrt $ v .* v

vector_angle :: Vector -> Vector -> Float
vector_angle v w = acos $ v .* w / (vector_length v * vector_length w)

instance Num Vector
 where
  v + w = Vector (x v + x w) (y v + y w) (z v + z w)
  v - w = Vector (x v - x w) (y v - y w) (z v - z w)
  v * w = Vector (y v * z w - z v * y w) (z v * x w - x v * z w) (x v * y w - y v * x w)
  negate v = Vector (- x v) (- y v) (- z v)
  abs v =  Vector (vector_length v) 0 0
  signum (Vector 0 0 0) = Vector 0 0 0
  signum v = Vector (x v / vector_length v) (y v / vector_length v) (z v / vector_length v)
  fromInteger i = Vector ((fromInteger :: Integer -> Float) i) 0 0

instance Show Vector
 where
  show v = "(x = " ++ (show . x $ v) ++ ", y = " ++ (show . y $ v) ++ ", z = " ++ (show . z $ v) ++ ")"

data Plane = Plane {point :: Coordinates, normal :: Vector}

plane :: Coordinates -> Coordinates -> Coordinates -> Plane
plane p q r = Plane p $ (q - p) * (r - p)

plane_angle :: Plane -> Plane -> Float
plane_angle p q = pi - vector_angle (normal p) (normal q)

vector_plane_angle :: Vector -> Plane -> Float
vector_plane_angle v = abs . (pi / 2 -) . vector_angle v . normal

plane_vector_angle :: Plane -> Vector -> Float
plane_vector_angle = flip vector_plane_angle

instance Show Plane
 where
  show p = "(point " ++ (show . point $ p) ++ ", normal = " ++ (show . normal $ p) ++ ")"

