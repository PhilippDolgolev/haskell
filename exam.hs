
data L a = N | O (L a) a (L a) | E (L a) (L a)


cons :: a -> L a -> L a
cons x N = O N x N
cons x (O h1 m h2) = E (x `cons` h1) (m `cons` h2)
cons x (E h1 h2) = O (x `cons` (dellast h1)) (takelast h1) h2
	where
		takelast (O N x N) = x
		takelast (O _ _ h2) = takelast h2 
		takelast (E _ h2) = takelast h2
		takelast N = error "impossible"
		dellast (O N _ N) = N
		dellast (O h1 m h2) = (E h1 (m `cons` (dellast h2)))
		dellast (E h1 h2) = O (dellast h1) (takelast h1) (dellast h2)


fromList :: [a] -> L a
fromList [] = N
fromList (x:xs) = x `cons` fromList xs

len :: L a -> Integer
len N = 0
len (O h1 m h2) = len h1 + 1 + len h2
len (E h1 h2) = len h1 + len h2

mapL :: (a->b) -> L a -> L b
mapL _ N = N
mapL f (O h1 m h2) = O (mapL f h1) (f m) (mapL f h2)
mapL f (E h1 h2) = E (mapL f h1) (mapL f h2)


foldL :: (a -> b -> b) -> L a -> b -> b
foldL f N acc = acc
foldL f (O h1 m h2) acc = foldL f h1 $ f m $ foldL f h2 acc
foldL f (E h1 h2) acc = foldL f h2 $ foldL f h1 acc



revert :: L a -> L a
revert N = N
revert (E h1 h2) = E (revert h2) (revert h1)
revert (O h1 m h2) = O (revert h2) m (revert h1) 



instance Show a => Show (L a) where
	show l = "[" ++ show' l ++"]"
		where 
			show' N = ""
			show' (O N x N) = show x
			show' (O h1 x h2) = show' h1 ++ "," ++ show x ++ "," ++ show' h2
			show' (E h1 h2) = show' h1 ++ "," ++ show' h2




class Enumerable a where
	enumeration :: [a]

instance Enumerable Integer where
	enumeration = [0] ++ gen 0 0
			where
				gen a b = [(a + 1), (b - 1)] ++ gen (a + 1) (b - 1)


mu f y = mu' f y enumeration
	where
		mu' f y [] = Nothing
		mu' f y (x:xs) | (f x) == y = Just x
		mu' f y (_:xs) = mu' f y xs


ndrop :: [a] -> [b] -> [(a,b)]

ndrop = dr [] []
dr ac1 ac2 [] [] = []
dr ac1 ac2@(y:ys) (x:xs) ([]) = [(x,y)] ++ [(x,i) | i <- ys] ++ dr (x:ac1) ac2 xs []
dr ac1@(x:xs) ac2 ([]) (y:ys) = [(x,y)] ++ [(i,y)| i<- xs] ++ dr ac1 (y:ac2) [] ys
dr ac1 ac2 (x:xs) (y:ys) = [(x, y)] ++ res ++ dr (x:ac1) (y:ac2) xs ys
	where res = [ (i, y)| i <- ac1 ] ++ [(x, j) | j <- ac2]


dp _ _ [] [] = []
dp [] [] (x:xs) (y:ys) = [(x, y)] ++ dp [x] [y] xs ys
dp a b (x:xs) [] = [(i, x) | i <- b] ++ dp a b xs []
dp a b [] (y:ys) = [(y, i) | i <- a] ++ dp a b [] ys
dp a b (x:xs) (y:ys) = res ++ dp (x:a) (y:b) xs ys 
	where
		res = [(i, x) | i <- b] ++ [(y, i) | i <- a]

dprod l m = dp [] [] l m