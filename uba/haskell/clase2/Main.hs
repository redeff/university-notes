mx :: (Ord a) => a -> a -> a
mx a b = if a < b then b else a

function3 :: Integer -> Integer -> Bool -> Bool
function3 x y b = b || (x > y)

doble :: (Num a) => a -> a
doble a = a + a

cuadruple :: (Num a) => a -> a
cuadruple = doble . doble

dist :: Float -> Float -> Float -> Float -> Float
dist a b c d = 5

e1 = doble 10
e2 =  dist (dist pi 0 pi 1) (doble 0) (doble 2) (3/4)
-- e3 = doble True -- No compila

esPar :: (Integral a) => a -> Bool
esPar = (`esMultiploDe` 2)

esMultiploDe :: (Integral a) => a -> a -> Bool
esMultiploDe a b =
	if a < 0 then esMultiploDe (-a) b else
	if 0 < a && a < b
	then False else
	if a == 0
	then True
	else esMultiploDe (a-b) b

esMultTres x
	| x == 0 = True
	| x == 1 = False
	| x == 2 = False
	| True   = esMultTres $ x - 3

triple x = x * 3

identidad :: Floating a => a -> a
identidad = (^2) . sqrt

normaVectorial :: (Float, Float) -> Float
normaVectorial v = sqrt $ (fst v) ^2 + (snd v) ^ 2

resta :: (Float, Float) -> (Float, Float) -> (Float, Float)
resta (a,b) (c,d) = (a-c, b-d)

distancia :: (Float, Float) -> (Float, Float) -> Float
distancia a b = normaVectorial $ resta a b

f1 :: Float -> (Float, Float, Float)
f1 x = (2*x, x^2, x - 7)

f2 :: (Integral a) => a -> a
f2 n = if esPar n then n `div` 2 else n + 1

f :: Integer -> Integer
f n = if n `esMultiploDe` 6 then (n^2) `div` 2 else 3*n+1

g :: (Integer, Integer) -> Integer
g (n, m) = n * (m+1)

h :: (Integer -> Integer) -> Integer
h = f . g
h p = f (g p)
