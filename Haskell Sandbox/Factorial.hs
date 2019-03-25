module Factorial where

factorial :: Integer -> Integer
factorial 1 = 1
factorial a = a * factorial (a-1)