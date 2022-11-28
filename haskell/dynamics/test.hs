{-# OPTIONS -Wall -Werror #-}

data Vector = Vector Float Float Float

instance Show Vector
 where
  show (Vector x y z) = "(x = " ++ (show x) ++ ", y = " ++ (show y) ++ ", z = " ++ (show z) ++ ")"

main :: IO ()
main =
 let
  vector_a = Vector 1 2 3
 in
  putStrLn . ("vector_a = " ++) . show $ vector_a

