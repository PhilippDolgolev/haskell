rnorm (a, b) = (a `div` d, b `div` d)
	where d = gcd a b

radd (a, b) (c, d) = rnorm (a * d + c * b , b * d)
rsub x (c, d) = radd x (-c, d) 
rmul (a, b) (c, d) = rnorm (a * c, b * d)
rinv (a, b) = rnorm (b, a)
rdiv x y = rmul x $ rinv y


map' f [] = []
map' f (x:xs) = f x : map' f xs


zip' l [] = []
zip' [] l = []
zip' (x:xs) (y:ys) = (x, y) : zip' xs ys

unzip' l = (map' first l, map' second l)
	where 
		first (a, b) = a
		second (a, b) = b

flatten [] = []
flatten (x:xs) = concat' x $ flatten xs 

concat' [] y = y 
concat' (x:xs) y = x: concat' xs y

inverse [] = []
inverse (x:xs) =  inverse xs `concat'` [x] 

