import qualified Data.List

data Lambda = Var String | App Lambda Lambda | Lam String Lambda
data LambdaDB = VarDB Int | AppDB LambdaDB LambdaDB | LamDB LambdaDB


names::[String]
names = tail $ generateName' [[]]
	where 
		generateName' prevLevel = prevLevel ++ (generateName' [ [y] ++ z | y <- ['a'..'z'], z <- prevLevel] )


toNameless = withLevel [] where
	withLevel env l =
		case l of
			Var x -> VarDB $ case Data.List.elemIndex x env of {Just i -> i; _ -> error ("free variable")}
			App x y -> AppDB (withLevel env x) (withLevel env y)
			Lam x l -> LamDB $ withLevel (x:env) l

toLambda = withNames [] names where
	withNames env m@(n:ns) l =
		case l of
			VarDB i -> Var $ env !! i
			AppDB x y -> App (withNames env m x) (withNames env m y)
			LamDB x -> Lam n (withNames (n:env) ns x)


shift' c d v@(VarDB k) | k < c = v
					 | otherwise = VarDB (k + d)
shift' c d (LamDB x) = LamDB (shift' (c + 1) d x)
shift' c d (AppDB x y) = AppDB (shift' c d x) (shift' c d y)

shift = shift' 0 1
downShift = shift' 0 (-1)

subst v@(VarDB i) j b | i == j = b
					  | otherwise = v
subst (AppDB m n) j b = AppDB (subst m j b) (subst n j b)
subst (LamDB a) j b = LamDB (subst a (j + 1) (shift b))


normReduct v@(VarDB x) = v
normReduct (LamDB x) = LamDB(normReduct x)
normReduct (AppDB (LamDB x) y) = downShift (subst x 0 (shift y)) --4
normReduct (AppDB a b) | notAbst a' = AppDB a' $ normReduct b	--3
	where
		a' = normReduct a
		notAbst (LamDB _) = False
		notAbst _ = True
normReduct (AppDB a b) = normReduct (normReduct a) b

cbn v@(VarDB x) = v
cbn v@(LamDB x) = v
cbn (AppDB a b) = subst a' 
	where a' = cbn a