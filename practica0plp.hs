sum1:: Num a => [a] -> a
sum1 = foldr(\x rec -> (+) x rec) 0


concatenar:: [a] -> [a] -> [a]
concatenar xs ys = foldr(\x rec -> x: rec) xs ys

--sumasParciales:: Num a => [a] -> [a]
--sumasParciales xs  = foldr(\x rec -> ) [] xs 

permutaciones :: [a] -> [[a]]
permutaciones [] = [[]]
permutaciones xs = concatMap (\i -> insertarEnTodasLasPosiciones (xs !! i) (take i xs ++ drop (i+1) xs)) [0..length xs - 1]

insertarEnTodasLasPosiciones :: a -> [a] -> [[a]]
insertarEnTodasLasPosiciones x ys = [ take i ys ++ [x] ++ drop i ys | i <- [0..length ys] ]

partes:: [a] -> [[a]]
partes = foldr (\x rec -> rec ++  map (x:) rec) [[]]

prefijos:: [a] -> [[a]]
prefijos = foldr (\x rec -> [[]] ++ map (x:) rec ) [[]]

elementosEnPosicionesPares :: [a] -> [a]
elementosEnPosicionesPares [] = []
elementosEnPosicionesPares (x:xs) = if null xs
then [x]
else x : elementosEnPosicionesPares (tail xs)

--no es estructural

entrelazar :: [a] -> [a] -> [a]
entrelazar [] = id
entrelazar (x:xs) = \ys -> if null ys
then x : entrelazar xs []
else x : head ys : entrelazar xs (tail ys)

recr::(a->[a]->b->b)->b->[a]->b
recr f z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

sublistas:: [a] -> [[a]]
sublistas = recr (\x xs rec -> [[]] ++ map (x:) (prefijos xs) ++ rec) [[]]
