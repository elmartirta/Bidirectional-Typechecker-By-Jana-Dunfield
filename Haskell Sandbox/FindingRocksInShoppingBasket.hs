import Test.QuickCheck

data ItemName = Rock String | Bowl String Int | Paper String | Scissors String | Knife deriving (Show, Eq)
type Price = Int

type ShopItem = (ItemName, Price)
type Basket   = [ShopItem]

sumPrices :: Basket -> Int
sumPrices [] = 0
sumPrices ((_, price):others) = price + sumPrices(others)

findRocks :: Basket -> Basket
findRocks [] = []
findRocks ((Rock a, p):others) = ((Rock a, p):(findRocks others))
findRocks (_          :others) = findRocks others


containsRocks :: Basket -> Bool
containsRocks b = findRocks b /= []

-- Find the first pair of scissors in a basket
---Split the basket into two baskets, where:
-- The Left  Basket contains all items left  of the scissors
-- The Right Basket contains all items right of the scissors

splitBasketUsingScissors :: Basket -> Basket -> (Basket, Basket)
splitBasketUsingScissors [] right_basket = ([], right_basket)
splitBasketUsingScissors ((Scissors _,_):left_basket)  right_basket = (left_basket, right_basket)
splitBasketUsingScissors (item:left_basket) right_basket = splitBasketUsingScissors left_basket (right_basket ++ [item])

splitBasketUsingKnife :: Basket -> (Basket, Basket)
splitBasketUsingKnife basket = go basket []
  where 
    go [] reversed_new = (reverse reversed_new, [])
    go ((Knife,_):old) reversed_new = (reverse reversed_new, old)
    go (item:old) reversed_new = go old (item:reversed_new)
    

-- Testing Basket [(Paper "A", 6), (Paper "B", 5),(Paper "C", 4),(Paper "D", 3), (Knife, 8),(Scissors "Henry", 7), (Paper "E", 2), (Paper "F", 1)]

-- Check out WinGHCI for pasting