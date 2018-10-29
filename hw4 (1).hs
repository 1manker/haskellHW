-- | Lucas Manker
-- | HW #4
-- | Cosc 3015
-- | 9/19/18


member :: (Eq a) => a -> [a] -> Bool
member x []     = False
member x (y:ys) = x == y || member x ys


pull :: Int -> [a] -> [a]
pull x xs 
   |x >= length xs = xs
   |otherwise      = pull x (init xs)

chop :: Int -> [a] -> [a]
chop 0 xs         = xs
chop x (y:ys)
   |x > length ys = []
   |otherwise     = chop (x - 1) ys

get :: Int -> [a] -> Maybe a
get 0 (x:xs)                       = Just x
get k (x:xs)
   |k < 0 || (k + 1) > (length xs) = Nothing
   |otherwise                      = get (k - 1) xs

find1 :: (a -> Bool) -> [a] -> Maybe a
find1 p []        = Nothing
find1 p (x:xs)
   |(p x)         = Just x
   |otherwise     = find1 p xs

find2 :: (a -> Bool) -> [a] -> [a]
find2 p []    = []
find2 p (x:xs) 
   |(p x)     = x:find2 p xs
   |otherwise = find2 p xs

test1_pull k xs =  let vs = pull k xs in
   if k >= length xs then (vs, vs == xs) else (vs, True)

test2_chop k xs = let vs = chop k xs in
   if k >= length xs then (vs, vs /= xs) else (vs, True)

test3_find k xs = (filter k xs) == (find2 k xs)

test4_get p k xs =
   case (find1 p xs) of
        Just x -> let j = length xs in
                       (Just x,  (get k xs == get (j - (k+1)) (reverse xs)) && 0 <= k && k < j )
        Nothing -> (Nothing, True)

