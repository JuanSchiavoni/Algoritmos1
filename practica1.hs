import Data.List
import Data.Char


{-
1. Definir las siguientes funciones y determinar su tipo
-}

-- a)five que dado cualquier valor, devuelve 5
five:: a -> Int
five x = 5

--b)apply que toma una func y un valor, y devuelve el resultado de aplicar la funcion al valor
apply:: (a -> a) -> a -> a
apply f x = f x

--c)id, la funcion identidad
ident:: a -> a
ident x = x

--d) first, que toma un par ordenado y devuelve la primer componente
first:: (a, a) -> a
first (x, _) = x 

--e)derive que aproxima la derivada de una funcion dada en un punto dado
derive:: Fractional a => (a -> a) -> a -> a
derive f x = fun / eps where 
    fun = f(x + eps) - f(x)
    eps = 0.00000001

--f) sign funcion signo
sign:: Int -> Int
sign x  =   if x < 0 then -1 else
            if x == 0 then 0 else 1


--g)abs, valor absoluto (usando sign y sim usarla)
vabs :: (Ord t, Num t) => t -> t
vabs n | n < 0 = negate n
       | True  = n


--h)pot, toma un entero y un numero, y devuelve el resultado de elevar el segundo 
-- a la potencia dada por el primero
pot:: Int -> Int -> Int
pot x y = (y ^ x) 


--i)xor, operador de disyuncion exclusiva
xor:: Bool -> Bool -> Bool
xor p q = (not $ p && q) && (not $ not p && not q)

--j)max3 toma 3 enteros y devuelve el mas grande
max3 :: Integral b => b -> b -> b -> b
max3 x y z = max x $ max y z

--k)swap toma un par y lo devuelve invertido
swap:: (a, a) -> (a, a)
swap (x, y) = (y, x)


{-
2.dar 2 ej de funciones que tengan los sig tipos
-}


--a) (Int -> Int) -> Int
poti:: (Int -> Int) -> Int
poti x y = (y * x) 


