import qualified Data.List

fromFun f argx = zip argx $ map f argx

dom = map fst 

eval f arg = case Data.List.elemIndex arg fx of {Just i -> fy !! i; _ -> error ("function not defined in this point")}
	where
		fx = fst f'
		fy = snd f'
		f' = unzip f

invert f = zip (snd f') (fst f')
	where
		f' = unzip f

infixr 9 .*.
(.*.) sn fs = zip fsx $ map (\ y -> case Data.List.elemIndex y snx of {Just i -> sny !! i; _ -> error("function not completle defind")}) fsy
	where
		fsx = fst fs'
		fsy = snd fs'
		snx = fst sn'
		sny = snd sn'
		fs' = unzip fs
		sn' = unzip sn

image f argx = foldl (\acc x -> case Data.List.elemIndex x fx of {Just i -> (fy !! i):acc; _ -> acc}) [] argx
	where
		fx = fst f'
		fy = snd f'
		f' = unzip f

preimage f argx = image (invert f) argx

isInjective f = length (Data.List.nub fy) == length f
	where fy = snd $ unzip f

isSurjective = True

areMutuallyInverse f g = (length f == length g) && (foldr (&&) True (foldr (\x acc -> (elem x f):acc) [] (invert g) ) )



prim' n d | n `div` 2 < d = True
prim' n d | n `mod` d == 0 = False
prim' n d = prim' n (succ d)
prim n = prim' n 2

allprim = [ x | x <- [1..], prim x]
