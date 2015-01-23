{-# LANGUAGE MultiParamTypeClasses #-}

-- кайндовая система типов, почитать

class M m where
	empty :: m a b
	add :: (Ord a) => m a b -> a -> b -> m a b
	del :: (Ord a) => m a b -> a ->  m a b
	find :: (Ord a) => m a b -> a -> Maybe b
	fold :: (a->b->c->c) -> m a b -> c -> c


--map на списке
newtype L a b = L [(a,b)]

instance M L where
 	empty = L []
 	fold f (L l) c = foldl (\ a b c -> f a b c) l c

 	add (L l) a b = L (insert l)
 		where 
 			insert l = 

 	del (L(_:xs)) k = L xs

 	find (L l) a = --lookup

 --map на отсортированном списке
-- д/з 

-- data E = X string | A E E| C Int
-- eval::(string->Int)->E->Int