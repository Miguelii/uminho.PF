
-- NOTA: Codigo tirado dos apontamentos das praticas, pode conter erros pois nao testei tudo 

-- 2a)
dobros :: [Float] -> [Float]
dobros [] = []
dobros (h:t) = (2*h):(dobros t)

-- b)
numOcorre :: Char -> String -> Int
numOcorre c[] = 0
numOcorre c (h:t) = if (c==h) then (1+numOcorre t)
	                          else  numOcorre t

-- c)
positivos :: [Int] -> Bool
positivos (h:t) = if (h>0) && positivos t

-- d)
soPos1 :: [Int] -> [Int]
soPos1 []=[]
soPos1 (h:t) = if (h<0) then soPos t
                       else (h:soPos t)

-- e)
somaNeg :: [Int] -> Int
somaNeg [] = 0
somaNeg (h:t) = if (h<0) then (h+somaNeg t)
                         else somaNeg t

-- f)
tresUlt :: [a] -> [a]
tresUlt [] = []
tresUlt (h:t) = if ( length (h:t)<=3 ) then (h:t)
                else tresUlt t

-- g)
segundos :: [(a,b)] -> [b]
segundos ((x,y):t) = y:segundos t
--OU
segundos1 :: [(a,b)] -> [b]
segundos1 []=[]
segundos1 (h:t) = (second h):segundos1 t

-- h)
nosPrimeiros :: (Eq a) => a -> [(a,b)] -> Bool
nosPrimeiros x [] = False
nosPrimeiros x ((y,z):t) | (x==y) = True 
                         | otherwise = x:nosPrimeiros z t

-- i)
sumTriplos :: (Num a, Num b, Num c) => [(a,b,c)] -> (a,b,c)
sumTriplos [] = (0,0,0)
sumTriplos [x] = x
sumTriplos ((x1,y1,z1):(x2,y2,z2):t) = sumTriplos ((x1+x2,y1+y2,z1+z2):t)


-- 4)
type Polinomio = [Monomio]
type Monomio = (Float,Int)
-- a)
conta :: Int -> Polinomio -> Int
conta l [] = 0
conta l ((x:y):t) | (l==y) = 1+conta l t
                  | (l/=y) = conta l t

-- b)
grau :: Polinomio -> Int
grau [(c,e)] = e
grau ((c1,l1):(c2,l2):t) | l1<l2 = ((c2,l2):t)
                         | otherwise = ((c1,l2):t)
-- c)
selgrau :: Int -> Polinomio -> Polinomio
selgrau e [] = []
selgrau e ((x:y):t) | (e==y) = (x:y):selgrau e t
                    | (e=/y) = selgrau e t
-- d)
deriv :: Polinomio -> Polinomio
deriv [] = []
deriv ((c1,l1):t) | (e==0) = deriv t
                  | (e/=0) = (c*(fromIntegral e), e-1):deriv t
-- e)
calcula :: Float -> Polinomio -> Float
calcula v [] = 0
calcula v ((c,e):t) = c*(v^e) + calcula v t

-- g)
mult :: Monomio -> Polinomio -> Polinomio
mult (c,e) [] = []
mult (c,e) ((c1,e1):t) = (c*c1,e*e1):mult (c,e) t

-- h)
normaliza :: Polinomio -> Polinomio
normaliza [] = []
normaliza [x] = [x]
normaliza ((c1,e1):(c2,e2):t) | (e1==e2) = normaliza ((c1+c2,e1):t)
                              | (e1/=e2) = (c1,e1):normaliza ((c2,e2):t)
-- i)
soma :: Polinomio -> Polinomio -> Polinomio
soma p1 p2 = normaliza (p1 ++ p2)
-- j)
 produto :: Polinomio -> Polinomio -> Polinomio
 produto _ [] = []
 produto [] _ = []
 produto p1 p2 = normaliza (prod p1 p2)
     where prod [] p2 = []
           prod ((c,e):p1) p2 = mult ( ((c,e) p2) ++ prod p1 p2)
-- k)
ordenaP :: Polinomio -> Polinomio
ordenaP [] = []
ordenaP ((c,e):t) = insere (c,e) (ordenaP t)
     where insere :: Monomio -> Polinomio -> Polinomio
           insere (c,e) [] = [(c,e)]
           insere (c,e) ((c1,e1):t) | e<e1 = (c,e):(c1,e1):t
                                    | otherwise = (c1,e1):(insere (c,e) t)
-- l)
equiv :: Polinomio -> Polinomio -> Bool
equiv p1 p2 = ordenaP ( normaliza p1) == ordenaP (normaliza p2)