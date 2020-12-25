maximum' :: (Ord a) => [a] -> a
maximum' []     = error "maximum of empyt list!"
maximum' [x]    = x
maximum' (x:xs) = max x (maximum' xs)

replicate' :: Int -> a -> [a]
replicate' n x
    | n <= 0    = []
    | otherwise = x : replicate' (n-1) x
    
take' :: (Num i, Ord i) => i -> [a] -> [a]
take' n _ 
    | n <= 0 = []
take' _ [] = []
take' n (x:xs) = x : take' (n-1) xs

quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = 
    let smallOrEqual = [a | a <- xs, a <= x]
        larger = [a | a <- xs, a > x]
    in quicksort smallOrEqual ++ [x] ++ quicksort larger

