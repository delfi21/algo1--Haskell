-- ejercicio 2.1
pertenece :: (Eq t) => t -> [t] -> Bool
pertenece _[] = False
pertenece x (y:ys) = x == y || pertenece x ys
-- ejercicio 2.4
hayrepetidos :: (Eq t) => [t] -> Bool
hayrepetidos [] = False
hayrepetidos (y:ys) = pertenece y (y:ys) || hayrepetidos ys  
--ejercicio 2.5 
quitar ::  (Eq t) => t -> [t] -> [t]
quitar _ [] = []
quitar x (y:ys) | x == y = ys 
                | otherwise = (y:quitar x ys)
-- ++ para concatenar listas 
-- ejercicio 3.3 
maximo :: (Ord t) => [t] -> t
maximo [x] = x
maximo (y:ys) | y < head(ys) = maximo (head(ys):ys)
              | y >= head(ys) = maximo (y:tail(ys))
--otra notacion (x:y:ys) y ahi tenes los dos primeros 
-- ejercicio 3.9 
ordenar :: (Ord t) => [t] -> [t]
ordenar [] = []
ordenar(x:xs)= (maximo(x:xs) : ordenar(quitar(maximo(x:xs)) (x:xs)) --revisar asi queda decreciente (pensar como hacerlo bien) (sale con ++ que concatena las funciones)