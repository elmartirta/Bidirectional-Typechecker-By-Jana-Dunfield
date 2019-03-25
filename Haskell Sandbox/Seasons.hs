data Season = Spring | Summer | Fall | Winter deriving (Show, Eq)
data Temperature = Hot | Warm | Cold deriving (Show, Eq)

seasonal_temperature :: Season -> Temperature
seasonal_temperature n
  | n == Spring = Warm
  | n == Summer = Hot
  | n == Fall   = Cold
  | n == Winter = Cold
