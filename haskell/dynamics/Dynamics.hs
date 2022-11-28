{-# OPTIONS -Wall -Werror #-}

module Dynamics where

data Vector = Vector Float Float Float

instance Show Vector
 where
  show (Vector x y z) = "(x = " ++ (show x) ++ ", y = " ++ (show y) ++ ", z = " ++ (show z) ++ ")"

