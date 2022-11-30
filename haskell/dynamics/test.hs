{-# OPTIONS -Wall -Werror #-}

import Dynamics

main :: IO ()
main =
 let
  vector_a = Vector 1 2 3
  vector_b = Vector 3 2 1
  vector_c = Vector 1 0 0
  vector_d = vector_a * vector_b
  plane_a = plane (coordinates 1 0 0) (coordinates 0 1 0) (coordinates 0 0 1)
  plane_b = plane (coordinates 1 2 3) (coordinates 4 5 6) (coordinates 9 8 7)
  posture_a = posture 4 5 6
  posture_b = front_up_2_posture vector_a vector_d
  state_a = State vector_a posture_a
  state_b = state 6 5 4 3 2 1
  a_position = coordinates 0 0 0
  a_front = Vector 1 1 1
  a_left = Vector (-1) 1 0
  a_up = a_front * a_left
  a_posture = front_up_2_posture a_front a_up
  a_state = State a_position a_posture
  b_position = coordinates 1 1 1
  b_front = Vector (-1) (-1) (-1)
  b_left = Vector 1 (-1) 0
  b_up = b_front * b_left
  b_posture = front_up_2_posture b_front b_up
  b_state = State b_position b_posture
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
  putStrLn . ("vector_length $ vector_a ->| plane_a <-| plane_a = " ++) . show . vector_length $ (vector_a ->| plane_a) <-| plane_a
  putStrLn . ("vector_angle (normal plane_a) $ vector_a - vector_a ->| plane_a = " ++) . show . vector_angle (normal plane_a) $ vector_a - vector_a ->| plane_a
  putStrLn . ("vector_a =>| plane_a = " ++) . show $ vector_a =>| plane_a
  putStrLn . ("plane_vector_angle plane_a $ vector_a =>| plane_a = " ++) . show . plane_vector_angle plane_a $ vector_a =>| plane_a
  putStrLn . ("rotate_vector (normal plane_a) (2 * pi / 3) vector_c = " ++) . show $ rotate_vector (normal plane_a) (2 * pi / 3) vector_c
  putStrLn . ("posture_a = " ++) . show $ posture_a
  putStrLn . ("posture_b = " ++) . show $ posture_b
  putStrLn . ("reverse_posture posture_a = " ++) . show $ reverse_posture posture_a
  putStrLn . ("reverse_posture posture_a @>@ posture_a = " ++) . show $ reverse_posture posture_a @>@ posture_a
  putStrLn . ("posture_a @>-@ posture_a = " ++) . show $ posture_a @>-@ posture_a
  putStrLn . ("posture_a @>-@ posture_b = " ++) . show $ posture_a @>-@ posture_b
  putStrLn . ("posture_a @>-@ posture_b @>@ posture_b = " ++) . show $ posture_a @>-@ posture_b @>@ posture_b
  putStrLn . ("posture_a @>-@ posture_b @>@ posture_b @>-@ posture_a = " ++) . show $ posture_a @>-@ posture_b @>@ posture_b @>-@ posture_a
  putStrLn . ("state_a = " ++) . show $ state_a
  putStrLn . ("state_b = " ++) . show $ state_b
  putStrLn . ("reverse_state state_a = " ++) . show $ reverse_state state_a
  putStrLn . ("state_a @<= state_a = " ++) . show $ state_a @<= state_a
  putStrLn . ("state_a =>@ state_b = " ++) . show $ state_a =>@ state_b
  putStrLn . ("state_a @<= state_b = " ++) . show $ state_a @<= state_b
  putStrLn . ("a_state = " ++) . show $ a_state
  putStrLn . ("b_state = " ++) . show $ b_state
  putStrLn . ("state_to_relative a_state a_state = " ++) . show $ state_to_relative a_state a_state
  putStrLn . ("state_to_relative b_state a_state = " ++) . show $ state_to_relative b_state a_state

