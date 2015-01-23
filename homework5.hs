data Lambda = Var String | App Lambda Lambda | Lam String Lambda
	deriving(Show)
fv (Var x) = [x]
fv (App x y) = fv x ++ fv y
fv (Lam x y) = filter (/= x) (fv y)

subst (Var x) (Var x') b | x == x' = b
subst (Var y) (Var x) b = Var y
subst (App m n) (Var x) b = App (subst m (Var x) b) (subst n (Var x) b)
subst (Lam x a) (Var x') b | x == x' = Lam x a
subst (Lam y a) (Var x) b = Lam y (subst a (Var x) b)

subst' (Var x) (Var x') b | x == x' = b
subst' (Var y) (Var x) b = Var y
subst' (App m n) (Var x) b = App (subst' m (Var x) b) (subst' n (Var x) b)
subst' (Lam x a) (Var x') b | x == x' = Lam x a
subst' (Lam y a) (Var x) b | not $ elem y (fv b) = Lam y (subst' a (Var x) b)
subst' (Lam y a) (Var x) b = subst' (Lam z $ subst' a (Var y) (Var z)) (Var x) b
	where
		z = [find (\x -> not (elem [x] (fv a) || elem [x] (fv b))) ['a'..] ]
		find f (x:xs) | f x = x
		find f (x:xs) = find f xs

reduction (Var x) = Var x
reduction (Lam x a) = Lam x $ reduction a 
reduction (App (Lam x a) b) = reduction (subst' a (Var x) b)
reduction (App a b) | notAbst a = reduction (subst' a' (Var x) b)
	where 
		notAbst (Lam x' a'') = False
		notAbst _ = True
		(Lam x a') = reduction a
reduction (App a b) | notAbst a' = App a' $ reduction b	
	where
		a' = reduction a
		notAbst (Lam x' a'') = False
		notAbst _ = True	