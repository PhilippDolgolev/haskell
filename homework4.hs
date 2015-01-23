mergeSort [] = []
mergeSort [x] = [x]
mergeSort x = merge (mergeSort $ take half x)  $ mergeSort $ drop half x
	where
		half = (length x ) `div` 2
		merge x [] = x
		merge [] y = y
		merge xa@(x:xs) ya@(y:ys) = if (x < y) then x:merge xs ya
							  else y:merge xa ys

data Tsil a = Lin | Snoc (Tsil a) a 

fromList :: [a] -> Tsil a
fromList [] = Lin
fromList (x:xs) = (fromList xs) `Snoc` x

toList :: (Tsil) a -> [a]
toList Lin = []
toList (xs `Snoc` x) = x : (toList xs)

length' :: Tsil a -> Int
length' (xs `Snoc` x) = 1 + length'(xs)
length' Lin = 0

map' :: (a -> b) -> Tsil a -> Tsil b
map' f Lin = Lin
map' f (xs `Snoc` x) = (map' f xs) `Snoc` f x

foldl' :: (b -> a -> b) -> b -> Tsil a -> b
foldl' _ acc Lin = acc
foldl' f acc (xs `Snoc` x) = f (foldl' f acc xs) x

foldr' :: (a -> b -> a) -> a -> Tsil b -> a
foldr' f acc Lin = acc
foldr' f acc (xs `Snoc` x) = foldr' f (f acc x) xs


reverse' :: Tsil a -> Tsil a
reverse' = foldr' Snoc Lin

concat' :: Tsil a -> Tsil a -> Tsil a
concat' x y = foldl' Snoc y x

flatten' :: Tsil (Tsil a) -> Tsil a
flatten' = foldr' concat' Lin