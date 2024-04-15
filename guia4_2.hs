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
               | otherwise = sumaimpares2(n-1)+(2*n-1)
 -- ejercicio 5
 medioFact :: Int -> Int 
 medioFact n | n == 0 || n == 1 = 1
             | otherwise = medioFact(n-2)*n 
--ejercicio 6
sumadigitos :: Int -> Int
sumadigitos n | n < 10 = n
 