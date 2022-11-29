{-# OPTIONS -Wall -Werror #-}

import qualified Dynamics

main :: IO ()
main =
 let
  vector_a = Dynamics.Vector 1 2 3
  vector_b = Dynamics.Vector 3 2 1
  plane_a = Dynamics.plane (Dynamics.coordinates 1 0 0) (Dynamics.coordinates 0 1 0) (Dynamics.coordinates 0 0 1)
  plane_b = Dynamics.plane (Dynamics.coordinates 1 2 3) (Dynamics.coordinates 4 5 6) (Dynamics.coordinates 9 8 7)
 in do
  putStrLn . ("vector_a = " ++) . show $ vector_a
  putStrLn . ("vector_b = " ++) . show $ vector_b
  putStrLn . ("vector_a + vector_b = " ++) . show $ vector_a + vector_b
  putStrLn . ("vector_a - vector_b = " ++) . show $ vector_a - vector_b
  putStrLn . ("vector_a * vector_b = " ++) . show $ vector_a * vector_b
  putStrLn . ("Dynamics.vector_angle vector_a vector_b = " ++) . show $ Dynamics.vector_angle vector_a vector_b
  putStrLn . ("negate vector_a = " ++) . show . negate $ vector_a
  putStrLn . ("abs vector_a = " ++) . show . abs $ vector_a
  putStrLn . ("signum $ Dynamics.Vector 0 0 0 = " ++) . show . signum $ Dynamics.Vector 0 0 0
  putStrLn . ("signum vector_a = " ++) . show . signum $ vector_a
  putStrLn . ("abs . signum $ vector_a = " ++) . show . abs . signum $ vector_a
  putStrLn . ("(fromInteger :: Integer -> Dynamics.Vector) 1 = " ++) . show . (fromInteger :: Integer -> Dynamics.Vector) $ 1
  putStrLn . ("plane_a = " ++) . show $ plane_a
  putStrLn . ("Dynamics.plane_vector_angle plane_a vector_a = " ++) . show $ Dynamics.plane_vector_angle plane_a vector_a
  putStrLn . ("Dynamics.vector_plane_angle vector_a plane_a = " ++) . show $ Dynamics.vector_plane_angle vector_a plane_a
  putStrLn . ("Dynamics.plane_angle plane_a plane_b = " ++) . show $ Dynamics.plane_angle plane_a plane_b

