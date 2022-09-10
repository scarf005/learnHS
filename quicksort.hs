import Data.List (partition)

-- | naive quicksort. not really fast in haskell because
-- of cons overhead and immutable lists
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (pivot : xs) =
  quicksort left ++ [pivot] ++ quicksort right
  where
    (left, right) = partition (< pivot) xs

main :: IO ()
main = do
  print $ quicksort [1, 3, 2, 5, 4, 7, 1, 12]
  print $ quicksort "the quick brown fox jumps over the lazy dog"
