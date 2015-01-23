module ParserCombinators where

newtype Parser s a = P (s -> [(a, s)])

apply :: Parser s a -> s -> [(a, s)]
apply (P p) = p

fail :: Parser s a
fail = P(\ _ -> [])
--всегда успешен, ничего не парсит, откусывает первый символ и ничего не парсит
any :: Parser [s] ()
any = P p where
	p [] = [((), [])]
	p (_:ss) = [((), ss)]

sym :: Eq s => s -> Parser [s] s
sym c = P p	
	where 
		p (x:xs) | x == c = [(x, xs)]
		p _ = []
--всегда возвращает парсерс, пропуская что ему дают ещё
lift :: Parser s a -> b -> Parser s a
lift p _ = p

test = sym 'a'
--обёртка
val :: a -> Parser s a
val a = P (\s -> [(a, s)])


infixl 2 |||
(|||) :: Parser s a -> Parser s a -> Parser s a
(P p1) ||| (P p2) = P (\s -> p1 s ++ p2 s)

infixl 3 ||>
(||>) :: Parser s a -> (a -> Parser s b) -> Parser s b
(P p) ||> f = P (\s -> concat [apply (f a) s | (a, s) <- p s])

--много раз применяем парсер к строке, 0 или более
many :: Parser s a -> Parser s [a]
many p = (p ||> \x -> many p ||> val . (x:)) ||| val []

-- походу строго больше 0
some :: Parser s a -> Parser s [a]
some a = a ||> (\x -> many a ||> val . (x:))

opt :: Parser s a -> Parser s (Maybe a)
opt a = (a ||> val . Just) ||| val Nothing

--возвращает распрасенный результат того, где дошло до конца
eof :: Eq s => [(a, [s])] -> [a]
eof = map fst . filter ((==[]) . snd)

--trs = eof $ apply (many (sym 'a')) "aaaaab"
--tr = apply (many (sym 'a')) "aaaaab"


--testeof = eof ()
--string :: String -> Parser String String

--string "" = val ""
--string (x:xs) = sym x ||> \_ -> 
--				string xs ||> \_b -> 
--				val (x:xs)

--string1 = many (string "abc")
--test = apply string1 "abcabcabc"

-- поиграться
--newtype N = N Integer 
--instance Eq N where
--	(==) (N a) (N b) = a == b