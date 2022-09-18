import Data.List

-- | Reverse Polish Notation calculator
solveRPN :: String -> Double
solveRPN = head . foldl fn [] . words
  where
    fn (x : y : ys) "*" = (x * y) : ys
    fn (x : y : ys) "+" = (x + y) : ys
    fn (x : y : ys) "-" = (y - x) : ys
    fn (x : y : ys) "/" = (y / x) : ys
    fn (x : y : ys) "^" = (y ** x) : ys
    fn (x : xs) "ln" = log x : xs
    fn xs "sum" = [sum xs]
    fn xs n = read n : xs

main = do
  let expr = "10 4 3 + 2 *"
  print $ solveRPN expr
