module Factorial where

factorial :: Integer -> Integer
factorial a = a * factorial (a-1)