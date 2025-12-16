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



--ejercicio2 d
algunoes0 :: Float -> Float -> Bool
algunoes0 x y = x==0 || y==0

-- ejercicio6
bisiesto :: Int -> Bool 
bisiesto x | mod x 4 == 0 = True
           | mod x 100 ==0 && mod x 400 /= 0 = True
           | otherwise = False
-- funcion_modulo
modulo :: Float -> Float 
modulo x | x>=0 = x
         | x<0 =(-x)

 --ejercicio 7
distancia :: (Float, Float, Float) -> (Float, Float, Float) -> Float
distancia (x1, y1, z1) (x2, y2, z2) = modulo (x2 - x1) + modulo (y2 - y1) + modulo (z2 - z1)

-- ejercicio 9 (a)
-- f1 (x: R) : R {
-- requiere: True
-- asegura: res=1 <-> x=0 y res=0 si x=/0
--}

{-(d)
f4(x:R, y:R): R {
requiere:True
asegura: res=x+y/2
-}

{-f
f6(x:R, y:Z): Bool{
    requiere:True
    asegura: res=True <-> la parte entera de x= y y res=false en otro caso
}-}

--ejercicio 3
estanrelacionados :: Int -> Int -> Bool
estanrelacionados x y = mod (x*x) (x*y) == 0

-- ejercicio 4 (e)
sumarmultiplos :: (Int,Int,Int) -> Int -> Int
sumarmultiplos (x, y, z) a | mod x a == 0 && mod y a == 0 && mod z a == 0 = x+y+z 
                       | mod x a == 0 && mod y a ==0 =x+y
                       | mod x a == 0 && mod z a ==0 =x+z 
                       | mod y a == 0 && mod z a == 0 = y+z 
                       | mod x a == 0 = x
                       | mod y a == 0 = y
                       | mod z a == 0 = z 
                       | otherwise = 0

                        




