maxl (x:xs) = foldl max x xs 
minl (x:xs) = foldl min x xs

isPrim n = isPrim' 2 where
	isPrim' d = d * d > n || (n `mod` d /= 0) && (isPrim' (d + 1))

primNumbers = [x | x <-[2..], isPrim x]

fibNumbers = 0:1:( zipWith (+) fibNumbers (tail fibNumbers) )