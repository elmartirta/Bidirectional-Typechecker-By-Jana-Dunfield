module SortList where

sortList :: [Integer] -> [Integer]
sortList l = insertionSort(l)

insertionSort :: [Integer] -> [Integer]
insertionSort [] = []

insertIntoList :: [Integer] -> Integer-> [Integer]
insertIntoList [] x = [x]
insertIntoList h:t x
  |x > h = h:insertIntoList(x t)
  |otherwise = x:h:t
