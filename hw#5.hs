-- |Lucas Manker
-- |HW #5
-- |9/25/18
-- |Cosc 3015

-- |Problem 1.1 Write a tail recursive version of the filter function using an accumulator.

filter' :: (a -> Bool) -> [a] -> [a]
filter' p []                    = []
filter' p xs                    = filter_acc p xs []
   where filter_acc p [] ys     = ys
         filter_acc p (x:xs) ys  
            |(p x)              = filter_acc p xs (ys ++ [x])
            |otherwise          = filter_acc p xs ys

-- |Problem 1.2 Write a tail recursive version of the rev fuction using an accumulator.

reverse' :: [a] -> [a]
reverse' []                = []
reverse' xs                = rev_acc xs []
   where rev_acc [] ys     = ys
         rev_acc (x:xs) ys = rev_acc xs (x:ys)

-- |Problem 1.3 Use fold1 to implement map, filter, and Data.List partition.

map' f = foldr (\x xs -> (f x):xs) []

filter'' p = foldr (\x xs -> if p x then x:xs else xs) []

partition' p = foldr (\x (ys, zs) -> if p x then (x:ys, zs) else (ys, x:zs)) ([], [])