-- ejercicio 1 n>=0
fib :: Integer -> Integer
fib n | n == 0 = 0
      | n == 1 = 1
      | otherwise = fib(n-1) + fib(n-2)
-- ejercicio 2 n>=0
parteEntera :: Float -> Integer
parteEntera x | x>=0 && x<1 = 0
              | otherwise = parteEntera(x-1) + 1

--ejercicio 3
esdivisible :: Integer -> Integer -> Bool 
esdivisible n1 n2 | n2 > n1 || n2 == 0 = False
                  | n1 == 0 = True
                  | otherwise = esdivisible (n1-n2) n2
--ejercicio 4 (sumo los anteriores a n )
sumaimpares :: Integer -> Integer 
sumaimpares n| mod n 2 /= 0 = 2*(n*(div (n+1) 2))-1
             | otherwise= sumaimpares(n-1)
-- suma impares de la guia 
sumaimpares2 :: Int -> Int 
sumaimpares2 n | n == 0 = 0
               | n == 1 = 1
               | otherwise = sumaimpares2(n-1)+(2*n-1) -- aca tengo un error 
 -- ejercicio 5
-- medioFact :: Int -> Int 
 --medioFact n | n == 0 || n == 1 = 1
             -- | otherwise = medioFact(n-2)*n 
--ejercicio 6
sumadigitos :: Int -> Int
sumadigitos n | n < 10 = n

--ejercicio 10 
f2 :: Int -> Float -> Float
f2 0 _ = 0
f2 _ 0 = 0
f2 n q = f2 (n-1) q + q^n 

f4 :: Int -> Float -> Float 
f4 0 _ = 0
f4 _ 0 = 0
f4 1 q = q^1+ q^2
f4 n q = f4 (n-1) q - q^(n-1) + q^(2*n-1)+ q^(2*n) 

-- ejercicio 11
-- defino primero la funcion factorial
factorial :: Integer -> Integer
factorial n | n == 0 = 1
            |otherwise = factorial (n-1)*n
--  definimos ahora la funcion que aproxima a e
aprox_e :: Integer -> Float 
aprox_e 0 = 1
aprox_e n = aprox_e(n-1)+(1/fromIntegral(factorial(n)))

-- ejercicio 12
sucesion :: Integer -> Float
sucesion 1 = 2
sucesion n = 2 + (1/sucesion(n-1))
raizde2Aprox :: Integer -> Float 
raizde2Aprox n = sucesion(n) - 1

-- ejercicio 13
sumaDoble :: Integer -> Float -> Float
sumaDoble m q n = sumaDoble(n-1)*m+f2(m q) 
--algo asi 
