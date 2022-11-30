{-# OPTIONS -Wall -Werror #-}

module Dynamics
(
 Vector(Vector),
 x,
 y,
 z,
 Coordinates,
 coordinates,
 (<=<=),
 (.*),
 vector_length,
 vector_angle,
 Plane(Plane),
 point,
 normal,
 (<-|),
 (->|),
 (=>|),
 plane,
 plane_angle,
 vector_plane_angle,
 plane_vector_angle,
 rotate_vector,
 Posture(Posture),
 posture_vectors,
 front_up_2_posture,
) where

import qualified Control.Monad

data Vector = Vector {x :: Double, y :: Double, z :: Double}

type Coordinates = Vector

coordinates :: Double -> Double -> Double -> Coordinates
coordinates = Vector

-- Scalar multiplication of vector
infixl 7 <=<=
(<=<=) :: Double -> Vector -> Vector
f <=<= v = Vector (f * x v) (f * y v) (f * z v)

-- Inner product of 2 vectors
infixl 8 .*
(.*) :: Vector -> Vector -> Double
v .* w = x v * x w + y v * y w + z v * z w

vector_length :: Vector -> Double
vector_length = sqrt . Control.Monad.join (.*)

vector_angle :: Vector -> Vector -> Double
vector_angle v w = acos $ v .* w / (vector_length v * vector_length w)

instance Num Vector
 where
  v + w = Vector (x v + x w) (y v + y w) (z v + z w)
  v - w = Vector (x v - x w) (y v - y w) (z v - z w)
  -- Cross product of 2 vectors
  v * w = Vector (y v * z w - z v * y w) (z v * x w - x v * z w) (x v * y w - y v * x w)
  negate v = Vector (- x v) (- y v) (- z v)
  abs v =  Vector (vector_length v) 0 0
  signum (Vector 0 0 0) = Vector 0 0 0
  signum v = Vector (x v / vector_length v) (y v / vector_length v) (z v / vector_length v)
  fromInteger i = Vector ((fromInteger :: Integer -> Double) i) 0 0

instance Show Vector
 where
  show v = "(x = " ++ (show . x $ v) ++ ", y = " ++ (show . y $ v) ++ ", z = " ++ (show . z $ v) ++ ")"

data Plane = Plane {point :: Coordinates, normal :: Vector}

-- Normal from plane to point
infixl 7 <-|
(<-|) :: Coordinates -> Plane -> Coordinates
c <-| p =
 let
  n = normal p
  d = c - point p
 in (d .* n / n .* n) <=<= n

-- Projection of point onto plane
infixl 7 ->|
(->|) :: Coordinates -> Plane -> Coordinates
c ->| p = c - c <-| p

-- Projection of vector onto plane
infixl 7 =>|
(=>|) :: Vector -> Plane -> Vector
v =>| p = v ->| p - (Vector 0 0 0) ->| p

plane :: Coordinates -> Coordinates -> Coordinates -> Plane
plane p q r = Plane p $ (q - p) * (r - p)

plane_angle :: Plane -> Plane -> Double
plane_angle p q = pi - vector_angle (normal p) (normal q)

vector_plane_angle :: Vector -> Plane -> Double
vector_plane_angle v = abs . (pi / 2 -) . vector_angle v . normal

plane_vector_angle :: Plane -> Vector -> Double
plane_vector_angle = flip vector_plane_angle

rotate_vector :: Vector -> Double -> Vector -> Vector
rotate_vector axis angle vector
 | vector_length axis == 0 = Vector 0 0 0
 | vector_length vector == 0 = vector
 | vector_angle vector axis == 0 = vector
 | otherwise =
    let
     plane_p = Plane (coordinates 0 0 0) axis
     vector_v = vector =>| plane_p
     vector_w = vector - vector_v
     vector_x = 1 / vector_length axis <=<= axis * vector_v
     vector_y = cos angle <=<= vector_v + sin angle <=<= vector_x
    in vector_w + vector_y

instance Show Plane
 where
  show p = "(point " ++ (show . point $ p) ++ ", normal = " ++ (show . normal $ p) ++ ")"

data Posture = Posture {roll :: Double, pitch :: Double, yaw :: Double}

posture_vectors :: Posture -> (Vector, Vector, Vector)
posture_vectors posture =
 let
  front = Vector 1 0 0
  left = Vector 0 1 0
  up = Vector 0 0 1
  rolled_front = rotate_vector front (roll posture) front
  rolled_left = rotate_vector front (roll posture) left
  rolled_up = rotate_vector front (roll posture) up
  pitched_front = rotate_vector rolled_left (pitch posture) rolled_front
  pitched_left = rotate_vector rolled_left (pitch posture) rolled_left
  pitched_up = rotate_vector rolled_left (pitch posture) rolled_up
  yawed_front = rotate_vector pitched_up (yaw posture) pitched_front
  yawed_left = rotate_vector pitched_up (yaw posture) pitched_left
  yawed_up = rotate_vector pitched_up (yaw posture) pitched_up
 in (yawed_front, yawed_left, yawed_up)

angle_error_limit :: Double
angle_error_limit = 2 * pi / 360

front_up_2_posture :: Vector -> Vector -> Posture
front_up_2_posture front up
 | vector_length front == 0 = Posture 0 0 0
 | vector_length up == 0 = Posture 0 0 0
 | angle_error_limit <= abs (pi / 2 - vector_angle front up) = Posture 0 0 0
 | otherwise =
    let
     front_unit = signum front
     up_unit    = signum up
     point_o = coordinates 0 0 0
     point_x = coordinates 1 0 0
     point_y = coordinates 0 1 0
     point_z = coordinates 0 0 1
     front_back_separator = plane point_o point_y point_z
     left_right_separator = plane point_x point_o point_z
     up_projection = up_unit =>| front_back_separator
     posture_roll
      | y up_projection == 0 = 0
      | y up_projection * z up_projection < 0 = vector_plane_angle up_projection left_right_separator
      | otherwise = negate $ vector_plane_angle up_projection left_right_separator
     rolled_up = rotate_vector point_x posture_roll point_z
     posture_pitch
      | 0 < x up_unit = vector_angle rolled_up up_unit
      | otherwise = negate $ vector_angle rolled_up up_unit
     rolled_left = rotate_vector point_x posture_roll point_y
     pitched_front = rotate_vector rolled_left posture_pitch point_x
     posture_yaw
      | vector_angle rolled_left front_unit <= pi / 2 = vector_angle pitched_front front_unit
      | otherwise = 2 * pi - vector_angle pitched_front front_unit
    in Posture posture_roll posture_pitch posture_yaw

instance Show Posture
 where
  show p = "(roll = " ++ (show . roll $ p) ++ ", pitch = " ++ (show . pitch $ p) ++ ", yaw = " ++ (show . yaw $ p) ++ ")"

