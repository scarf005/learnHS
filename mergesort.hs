mergesort :: (Ord a) => [a] -> [a]
mergesort [] = []
mergesort [x] = [x]
mergesort xs =
  merge (mergesort left) (mergesort right)
  where
    (left, right) = splitHalf xs

-- | Split a list into two halves.
splitHalf :: [a] -> ([a], [a])
splitHalf xs = splitAt size xs
  where
    size = length xs `div` 2

-- | Merge two sorted lists into one sorted list.
merge :: (Ord a) => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge xall@(x : xs) yall@(y : ys)
  | x <= y = x : merge xs yall
  | otherwise = y : merge xall ys

main = do
  print $ mergesort [1, 3, 2, 5, 4, 7, 1, 12]
  print $ mergesort "the quick brown fox jumps over the lazy dog"
