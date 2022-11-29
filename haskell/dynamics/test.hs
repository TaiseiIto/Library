{-# OPTIONS -Wall -Werror #-}

import qualified Dynamics

main :: IO ()
main =
 let
  vector_a = Dynamics.Vector 1 2 3
  vector_b = Dynamics.Vector 3 2 1
 in do
  putStrLn . ("vector_a = " ++) . show $ vector_a
  putStrLn . ("vector_b = " ++) . show $ vector_b
  putStrLn . ("vector_a + vector_b = " ++) . show $ vector_a + vector_b
  putStrLn . ("vector_a - vector_b = " ++) . show $ vector_a - vector_b
  putStrLn . ("vector_a * vector_b = " ++) . show $ vector_a * vector_b
  putStrLn . ("Dynamics.vecarg vector_a vector_b = " ++) . show $ Dynamics.vecarg vector_a vector_b
  putStrLn . ("negate vector_a = " ++) . show . negate $ vector_a
  putStrLn . ("abs vector_a = " ++) . show . abs $ vector_a
  putStrLn . ("signum $ Dynamics.Vector 0 0 0 = " ++) . show . signum $ Dynamics.Vector 0 0 0
  putStrLn . ("signum vector_a = " ++) . show . signum $ vector_a
  putStrLn . ("abs . signum $ vector_a = " ++) . show . abs . signum $ vector_a
  putStrLn . ("(fromInteger :: Integer -> Dynamics.Vector) 1 = " ++). show . (fromInteger :: Integer -> Dynamics.Vector) $ 1

