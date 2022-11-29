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
  negate v = Vector (- x v) (- y v) (- z v)
  abs v =  Vector (veclen v) 0 0
  signum v = Vector (x v / veclen v) (y v / veclen v) (z v / veclen v)
  fromInteger i = Vector ((fromInteger :: Integer -> Float) i) 0 0

instance Show Vector
 where
  show v = "(x = " ++ ((show . x) v) ++ ", y = " ++ ((show . y) v) ++ ", z = " ++ ((show . z) v) ++ ")"

