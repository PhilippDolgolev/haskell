newtype Parser s a = P ( s -> [(a, s)])

fail = P (\s -> [])


any' = P p where
	p [] = [((), [])]
	p (_:ss) = [((), ss)]


sym c = P p where
	p (x:xs) | x == c = [(x, xs)]
	p _ = []

val a = P p where
	p x = [(a, x)]

infixl 2 |||

(|||) a b = P p where
	p x = (a x) ++ (b x)

sh = [x | x <- [100, 101.. 999], (((x `mod` 10) == 2) || ((x `mod` 10) == 0) || ((x `mod` 10) == 1) || ((x `mod` 10) == 5)) && (((x `div` 100) == 2) || ((x `div` 100) == 0) || ((x `div` 100) == 1) || ((x `div` 100) == 5)) && ((((x `div` 10) `mod` 10) == 2) || (((x `div` 10) `mod` 10) == 0) || (((x `div` 10) `mod` 10) == 1) || (((x `div` 10) `mod` 10) == 5))]

apply:: Parser s a -> s -> [(a, s)]
apply (P p) = p 
-- aplly
-- lift
-- ||>
-- many
-- some
--
--