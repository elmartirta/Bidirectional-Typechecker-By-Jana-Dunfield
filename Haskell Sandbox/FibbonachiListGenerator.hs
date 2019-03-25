fib :: Int -> [Int]
fib n
  | n <= 0 = error ("There is no fibonacchi sequence of length" ++ show n)
  | n == 1 = [1]
  | n == 2 = [1, 1]
  | otherwise = a+b:res
  where 
    res = fib (n-1)
    a = head (res)
    b = head (tail res)