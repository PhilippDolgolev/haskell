data E = X String | C Int | A E E | S E E | M E E | D E E
data R a = Ok a | Fail String
instance Monad R where
		return = Ok
		(Ok x) >>= f = f x
		(Fail s) >>= _ = Fail s
eval s (X n) = s n
eval _ (C n) = Ok n
--eval s (A x y) = (>>=) (eval s x) (\ x' -> 
--				 (>>=) (eval s y) (\ y' -> 
--				 return $ x' + y'))
eval s (A x y) = do
	x' <- eval s x
	y' <- eval s y
	return $ x' + y'
eval s (S x y) = (>>=) (eval s x) (\ x' -> 
				 (>>=) (eval s y) (\ y' ->
				 return $ x' - y'))
eval s (M x y) = (>>=) (eval s x) (\ x' -> 
				 (>>=) (eval s y) (\ y' ->
				 return $ x' * y'))
eval s (D x y) = (>>=) (eval s x) (\ x' -> 
				 (>>=) (eval s y) (\ y' ->
				 if y' == 0 then Fail "Division by zero" else return $ x' `div` y'))
--монада в хаскелле класс типов
-- * -> *
 --монада вычилсение, которое возвращает a
-- bind заменить на >>=

-- e>>=(\x -> E
--	do x <- e E) // do нотация
--монадические законы