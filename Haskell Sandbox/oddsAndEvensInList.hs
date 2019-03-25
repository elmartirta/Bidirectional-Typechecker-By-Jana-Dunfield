
oddsInList :: [Int] -> [Int]
oddsInList [] = []
oddsInList (num:others)
  | condition =  [num] ++ oddsInList others
  | otherwise = oddsInList others
  where condition = (num `mod` 2 == 1)
  
evensInList :: [Int] -> [Int]
evensInList [] = []
evensInList (num:others)
  | condition = [num] ++ oddsInList others
  | otherwise = oddsInList others
  where condition = (num `mod` 2 == 0)
  
  
test1 = oddsInList [1,2,3,4] == [1,3]
test2 = evensInList [1,2,3,4] == [2,4]