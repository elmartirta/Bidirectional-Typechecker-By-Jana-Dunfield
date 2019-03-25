module FooBarGenerator where
import Data.Char

fooBar :: Int -> [String]
fooBar 1 = ["1"]
fooBar x 
  | (mod x 15 == 0) = fooBar(x-1) ++ [show "FooBar"]
  | (mod x 5 == 0) = fooBar(x-1) ++ [show "Bar"] 
  | (mod x 3 == 0) = fooBar(x-1) ++ [show "Foo"] 
  | otherwise  = fooBar(x-1) ++ [show x]