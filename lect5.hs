data Lambda = Var String | App Lambda Lambda | Lam String Lambda

-- show' :: Lambda -> String
-- show' (Lam "x" (App (Var "y") ( Var "x"))) --> "\xyx"
--show (App (Lam "x" (Var "y")) (Var "x")) —-> "(\xy)x"
--App ( Lam "x" (Var "x")) (App (var "y") (Var "z"))

show' (Var x) = x
show' (Lam x y) = "\\" ++ x ++ show' y
show' (App x y) = (lam_x $ show' x) ++ (app_y $ show' y) where
	lam_x = case x of Lam _ _ -> br; _ -> id
	app_y = case y of App _ _ -> br; _ -> id
	br x = "(" ++ x ++ ")"

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


-- доделать дома
-- написать 3 функции:
--1. alpha , сделать subst', там всё так же кроме случая
-- (\yA)[x <- b] 
-- 1 случай, y не принадлежит множеству свободных переменных, тогда \y(A[x<-b])
-- 2 случай, y входит, тогда взять переменную которая ни входит ни в а ни в б, (\z(A[y<-z]))[x<-b] где z не входит ни в a ни в b (нужны будут свежие переменные)
-- 1 случай (\xA)B -> A[x <- B] полная редукция
-- A => A', тогда \xA =xA'
-- a) A => A'
-- \xA => \xA'
-- b) A => \xA', A'[x<-b] => B' (смотреть снизу вверх)
-- AB => B'
-- с) A => A', B => B'
-- AB +> A'B'
-- в) x => x
