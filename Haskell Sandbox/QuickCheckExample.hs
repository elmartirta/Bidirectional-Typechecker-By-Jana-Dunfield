import Test.QuickCheck

-- Square function, computes square
square :: Int -> Int
square x = x * x

-- Not Negative function, asserts not negative
notNegative :: Int -> Bool
notNegative a = (a >= 0)

-- QuickCheck checks to see if every (square x) is not negative
-- Run by typing >testSquare
testSquare = quickCheck (\x ->  notNegative (square x))