--1)un numero perfecto es aquel que es igual a la suma de sus divisores menores que el

--divisores hace una lista de los divisores de d con resto 0
--perfectos hace una lista solo con los numeros perfectos

divisores :: Int -> [Int]
perfectos :: Int -> [Int]

divisores d = [x | x <- [1..d], d `mod`x == 0]
perfectos n = [x | x <- [1..n], sum (init (divisores x)) == x]


--2)Escribir una funcion que recibe como argumento dos listas ordenadas
--y devuelve una lista ordenada de fusion de las listas

juntar :: (Ord a) => [a] -> [a] -> [a]
juntar [] a = a
juntar b [] = b
juntar (x:xs) (y:ys) = if x < y then (x : juntar xs (y:ys)) else (y : juntar ys (x:xs))


--3)escriba una funcion quicksort sin utilizar listas por comprension
-- escriba uan funcion particion que reciba un pivote y una lista de valores


partition :: (Ord a) => a -> [a] -> ([a],[a])
partition p [] = ([],[])
partition p (x:xs) = if x <= p then (x:l1,l2) else (l1,x:l2)
                     where (l1,l2) = partition p xs


qsort :: (Ord a) => [a] -> [a]
qsort [] = []
qsort [a] = [a]
qsort (x:xs) = qsort y ++ [x] ++ qsort ys
               where (y,ys) = partition x xs 


--4)ZIP recibe como argumento dos listas (x:xs) e (y:ys) y produce lista de tuplas (i,j)
--escriba una funcion personal de mizip
--utilizando mizip escriba el producto escalar de 2 listas


miZip :: [a] -> [b] -> [(a,b)]
miZip _ [] = []
miZip [] _ = []
miZip (x:xs) (y:ys) = (x,y) : miZip xs ys


--5)

prodEscalar :: [Int] -> [Int] -> Int
prodEscalar xs ys = sum[ x*y | (x,y) <- miZip xs ys]


--6)

indexado :: [a] -> [(a,Int)]
indexado lista = miZip lista [1..]



--7)

inserta :: (Ord a) => a -> [a] -> [a]
inserta a [] = [a]
inserta a (x:xs) = if a < x then (a:x:xs) else (x: inserta a xs)



--8)

split :: (Ord a) => a -> [a] -> ([a],[a])
split x xs = foldl aux ([],[]) xs
             where aux (ys,zs) w | w <= x = (w:ys,zs)
                                 | otherwise = (ys, w:zs)


--TDAs-----------------------------------

--Arbol Binario

data ArbolBin a = Empty | Nodo a (ArbolBin a) (ArbolBin a) deriving Show

newTree :: ArbolBin a
newTree = Empty

addTree :: (Ord a) => a -> ArbolBin a -> ArbolBin a
addTree x Empty = (Nodo x) (Empty) (Empty)
addTree x (Nodo y (lt) (rt)) | x == y = (Nodo y) (lt) (rt)
                             | x < y = (Nodo y) (addTree x lt) (rt)
                             | x > y = (Nodo y) (lt) (addTree x rt)

inOrderTree :: (Ord a) => ArbolBin a -> [a]
inOrderTree Empty = []
inOrderTree (Nodo x (lt) (rt)) = inOrderTree lt ++ [x] ++ inOrderTree rt


--Cola de Prioridad

newtype ColaPrio a = CP[a] deriving Show

mkqpr :: ColaPrio a
mkqpr = CP []

addqpr :: a -> ColaPrio a -> ColaPrio a
addqpr a (CP p) = CP (p ++ [a])

nextqpr :: ColaPrio a -> a
nextqpr (CP []) = error "Cola vacia"
nextqpr (CP (x:xs)) = x

popqpr :: ColaPrio a -> ColaPrio a
popqpr (CP []) = error "Cola vacia"
popqpr (CP (x:xs)) = CP xs



--Set o Conjunto
                    
--module Set (Set, emptySet, setEmpty, addSet, inSet, delSet) where

   --newtype Set a = St [a] deriving Show

   --emptySet :: Set a
   --emptySet = St []

   --setEmpty :: Set a -> Bool
   --setEmpty (St []) = True
   --setEmpty (St _) = False

   --inSet :: (Eq a) => a -> Set a -> Bool
   --inSet x (St []) = False
   --inSet x (St (y:ys)) = x == y || inSet x (St (ys))

   --addSet :: (Eq a) => a-> Set a -> Set a
   --addSet x (St xs) = if inSet x (St xs) then St (xs) else St (x:xs)

   --delSet :: (Eq a) => a -> Set a -> Set a
   --delSet x (St []) = St []
   --delSet x (St (y:ys)) = if x /= y then addSet y (delSet x (St (ys))) else delSet x (St (ys))

   --unionSet :: (Eq a) => Set a -> Set a -> Set a
   --unionSet (St []) (St (ys)) = St (ys)
   --unionSet (St (x:xs)) (St (ys)) = if inSet x (St (ys)) then unionSet (St (xs)) (St (ys)) else unionSet (St (xs)) (St (x:ys))
