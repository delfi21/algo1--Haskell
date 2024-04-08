-- prueba
doubleMe :: Int -> Int
doubleMe x = x + x

-- ejercicio 1
f :: Int -> Int
f n | n == 1 = 8
    | n == 4 = 131
    | n ==16 = 16

g :: Int -> Int 
g n | n == 8 = 16
    | n == 16 = 4
    | n == 131 = 1

h :: Int -> Int
h x = f(g x)

-- ejercicio 2
maximo3 :: Int -> Int -> Int -> Int
maximo3 x y z | (x >= y) && (x >= z) =x
              | (y >= z) =y
              | otherwise =z

-- ejercicio 3
sumadistintos :: Int -> Int -> Int -> Int
sumadistintos x y z | (x /= y) && (x /= z) && (y /= z)= x+y+z
                    | (x==y) && (x /= z) =z
                    | (x==z) && (x/=y) =y
                    | (y == z) && (x/=y) =x
                    | otherwise = 0

-- ejercicio 4
digitounidades :: Int -> Int
digitounidades n | n>=0 = mod n 10 
                 |otherwise = mod (-n) 10

-- ejercicio 5
digitodecenas :: Int -> Int
digitodecenas n | n>=0 = digitounidades (div n 10) 
                | n<=0 = digitounidades(div (-n) 10) 








