son_iguales :: (String,String) -> Bool
son_iguales(x,y) | x==y = False
                 | otherwise = True

elementos_iguales :: (String, String) -> (String, String) -> Bool
elementos_iguales (x,y) (a,b) | (x,y) == (a,b) || x==b && y == a = False
                              | otherwise = True 

f1 :: [(String,String)] -> Bool
f1 [] = True 
f1(x:y:ys) | son_iguales x == False = False 
           | otherwise = f1(y:head(ys):tail(ys))

f2 ::  [(String,String)] -> Bool
f2 [] = True 
f2(x:y:ys) | elementos_iguales x y == False = False 
           | otherwise = f2(x:head(ys):tail(ys))

f3 ::  [(String,String)] -> Bool
f3 [] = True 
f3(x:y:ys) | f2(x:y:ys) == False = False 
            | otherwise = f2(y:head(ys):tail(ys))

relacionesValidas :: [(String, String)] -> Bool 
reacionesValidas[] = True
relacionesValidas(x:y:ys) | f1(x:y:ys) == False || f3(x:y:ys) == False = False 
                          | otherwise = True -- bueno lo corrijo en casa +