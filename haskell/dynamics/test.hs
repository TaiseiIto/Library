{-# OPTIONS -Wall -Werror #-}

import Dynamics

main :: IO ()
main =
 let
  vector_a = Vector 1 2 3
  vector_b = Vector 3 2 1
  vector_c = Vector 1 0 0
  plane_a = plane (coordinates 1 0 0) (coordinates 0 1 0) (coordinates 0 0 1)
  plane_b = plane (coordinates 1 2 3) (coordinates 4 5 6) (coordinates 9 8 7)
 in do
  putStrLn . ("vector_a = " ++) . show $ vector_a
  putStrLn . ("vector_b = " ++) . show $ vector_b
  putStrLn . ("vector_a + vector_b = " ++) . show $ vector_a + vector_b
  putStrLn . ("vector_a - vector_b = " ++) . show $ vector_a - vector_b
  putStrLn . ("vector_a * vector_b = " ++) . show $ vector_a * vector_b
  putStrLn . ("vector_angle vector_a vector_b = " ++) . show $ vector_angle vector_a vector_b
  putStrLn . ("negate vector_a = " ++) . show . negate $ vector_a
  putStrLn . ("abs vector_a = " ++) . show . abs $ vector_a
  putStrLn . ("signum $ Vector 0 0 0 = " ++) . show . signum $ Vector 0 0 0
  putStrLn . ("signum vector_a = " ++) . show . signum $ vector_a
  putStrLn . ("abs . signum $ vector_a = " ++) . show . abs . signum $ vector_a
  putStrLn . ("(fromInteger :: Integer -> Vector) 1 = " ++) . show . (fromInteger :: Integer -> Vector) $ 1
  putStrLn . ("plane_a = " ++) . show $ plane_a
  putStrLn . ("plane_vector_angle plane_a vector_a = " ++) . show $ plane_vector_angle plane_a vector_a
  putStrLn . ("vector_plane_angle vector_a plane_a = " ++) . show $ vector_plane_angle vector_a plane_a
  putStrLn . ("plane_angle plane_a plane_b = " ++) . show $ plane_angle plane_a plane_b
  putStrLn . ("vector_a ->| plane_a = " ++) . show $ vector_a ->| plane_a
  putStrLn . ("vector_length $ vector_a ->| plane_a <-| plane_a = " ++) . show . vector_length $ vector_a ->| plane_a <-| plane_a
  putStrLn . ("vector_angle (normal plane_a) $ vector_a - vector_a ->| plane_a = " ++) . show . vector_angle (normal plane_a) $ vector_a - vector_a ->| plane_a
  putStrLn . ("vector_a =>| plane_a = " ++) . show $ vector_a =>| plane_a
  putStrLn . ("plane_vector_angle plane_a $ vector_a =>| plane_a = " ++) . show . plane_vector_angle plane_a $ vector_a =>| plane_a
  putStrLn . ("rotate_vector (normal plane_a) (2 * pi / 3) vector_c = " ++) . show $ rotate_vector (normal plane_a) (2 * pi / 3) vector_c

