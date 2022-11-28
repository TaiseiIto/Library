{-# OPTIONS -Wall -Werror #-}

module Dynamics where

data Vector = Vector {x :: Float, y :: Float, z :: Float}

infix 7 .*
(.*) :: Vector -> Vector -> Float
(Vector x0 y0 z0) .* (Vector x1 y1 z1) = x0 * x1 + y0 * y1 + z0 * z1

veclen :: Vector -> Float
veclen v = sqrt $ v .* v

instance Num Vector
 where
  (Vector x0 y0 z0) + (Vector x1 y1 z1) = Vector (x0 + x1) (y0 + y1) (z0 + z1)
  (Vector x0 y0 z0) - (Vector x1 y1 z1) = Vector (x0 - x1) (y0 - y1) (z0 - z1)
  (Vector x0 y0 z0) * (Vector x1 y1 z1) = Vector (y0 * z1 - z0 * y1) (z0 * x1 - x0 * z1) (x0 * y1 - y0 * x1)
  negate (Vector x y z) = Vector (-x) (-y) (-z)
  abs v =  Vector (veclen v) 0 0
  signum
  fromInteger i = Vector i 0 0

instance Show Vector
 where
  show (Vector x y z) = "(x = " ++ (show x) ++ ", y = " ++ (show y) ++ ", z = " ++ (show z) ++ ")"

