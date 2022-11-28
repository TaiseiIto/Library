{-# OPTIONS -Wall -Werror #-}

import qualified Dynamics

main :: IO ()
main =
 let
  vector_a = Dynamics.Vector 1 2 3
 in
  putStrLn . ("vector_a = " ++) . show $ vector_a

