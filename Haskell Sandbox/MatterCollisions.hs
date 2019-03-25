type Size = Int
type Name = String
data Element = Matter Name Size | AntiMatter Name Size | Space Name Sizederiving (Show, Eq)
-- Matter destroys AntiMatter, AntiMatter destroys Matter, Space is neutral, neither destroying or destroyed

collide :: [Element] -> [Element]
collide a = collideHelper a []

collideHelper :: [Element] -> [Element] -> [Element] 
collideHelper [] bucket = bucket
collideHelper (Matter a b:others) [] = collideHelper others [Matter a b]
collideHelper (AntiMatter a b:others) [] = collideHelper others [AntiMatter a b]
collideHelper (Matter a a_size:a_others)     (Matter b b_size:b_others)     = collideHelper a_others ([Matter b b_size]     ++ [Matter a a_size]     ++ b_others)
collideHelper (AntiMatter a a_size:a_others) (AntiMatter b b_size:b_others) = collideHelper a_others ([AntiMatter b b_size] ++ [AntiMatter a a_size] ++ b_others)
collideHelper _ _ = undefined
--collide (AntiMatter name size:xs) (AntiMatter name size:xs) = 





test1 = collide [] == []
test2 = collide [Matter "Elephant" 20] == [Matter "Elephant" 20]
test3 = collide [AntiMatter "Anti-Elephant" 20] == [AntiMatter "Anti-Elephant" 20]
test4 = collide [Matter "Elephant" 20, Matter "Grass" 5] == [Matter "Elephant" 20, Matter "Grass" 5] 
test5 = collide [AntiMatter "Anti-Elephant" 20, AntiMatter "Anti-Grass" 5] == [AntiMatter "Anti-Elephant" 20, AntiMatter "Anti-Grass" 5]
test6 = collide [Matter "Elephant" 20, AntiMatter "Anti-Elephant" 20] == []
test7 = collide [Matter "Elephant" 20, AntiMatter "Grass" 5] == [Matter "Elephant" 15]
test8 = collide [Matter "Elephant" 20, AntiMatter "Grass" 5, AntiMatter "Grass" 5] == [Matter "Elephant" 10]
test9 = collide [Matter "Elephant" 20, AntiMatter "Grass" 5, AntiMatter "Grass" 5, AntiMatter "Grass" 5, AntiMatter "Grass" 5] == []
test10 = collide [Matter "Elephant A" 20, Matter "Elephant B" 20, AntiMatter "Truck" 30] == [Matter "Elephant B" 10]

test11 = collide [Matter "Elephant" 20, Space "Void" 20, AntiMatter "Anti-Elephant" 20] == [Space "Void" 20]