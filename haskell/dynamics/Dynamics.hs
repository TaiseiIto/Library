{-# OPTIONS -Wall -Werror #-}

module Dynamics where

data Vector = Vector {x :: Float, y :: Float, z :: Float}

infixl 7 .*
(.*) :: Vector -> Vector -> Float
v .* w = x v * x w + y v * y w + z v * z w

veclen :: Vector -> Float
veclen v = sqrt $ v .* v

vecarg :: Vector -> Vector -> Float
vecarg v w = acos $ v .* w / (veclen v * veclen w)

instance Num Vector
 where
  v + w = Vector (x v + x w) (y v + y w) (z v + z w)
  v - w = Vector (x v - x w) (y v - y w) (z v - z w)
  v * w = Vector (y v * z w - z v * y w) (z v * x w - x v * z w) (x v * y w - y v * x w)
  negate v = Vector (- x v) (- y v) (- z v)
  abs v =  Vector (veclen v) 0 0
  signum (Vector 0 0 0) = Vector 0 0 0
  signum v = Vector (x v / veclen v) (y v / veclen v) (z v / veclen v)
  fromInteger i = Vector ((fromInteger :: Integer -> Float) i) 0 0

instance Show Vector
 where
  show v = "(x = " ++ (show . x $ v) ++ ", y = " ++ (show . y $ v) ++ ", z = " ++ (show . z $ v) ++ ")"

