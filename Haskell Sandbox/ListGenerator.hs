module ListGenerator where

generateList :: Integer -> [Integer]
generateList 1 = [1]
generateList x = x:generateList(x-1)