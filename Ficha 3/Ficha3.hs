-- FICHA 3
type Poligonal = [Ponto]
data Figura = Circulo Ponto Double
            | Rectangulo Ponto Ponto
            | Triangulo Ponto Ponto Ponto
            deriving (Show,Eq)

-- a)
comp :: Poligonal -> Bool
comp (p1:p2:t) = (Ponto p1 p2) + comp(p2:t)
-- b)
efechada :: Poligonal -> Bool
efechada [] = False
efechada l = head l == last l
-- c)
triangula :: Poligonal -> [Figura]
triangula (p1:p2:p3:p4:t) = (triangula p1 p2 p3):(triangula (p1:p3:p4:t))
triangula _ = []
-- d)
areaP :: Poligonal -> Double
areaP lp = let lt = Triangulo
           in somaarea lt
somaarea :: [Figura] -> Double
somaarea [] = 0
somaarea (h:t) = (area h) + somaarea t

 -- 3)
 data Contacto = Casa Integer
               | Trab Integer
               | Tlm Integer
               | Email String
               deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

-- a)
acrescEmail :: Nome -> String -> Agenda -> Agenda
acrescEmail no em [] = [(no,[Email em])]
acrescEmail no em ((n,lc):ag)
            | no /= n = (n,lc):(acrescEmail no em ag)
            | no == n = (n,(Email em):lc):ag

-- b)
verEmails :: Nome -> Agenda -> Maybe [String]
verEmails _ [] = Nothing
verEmails no ((n,lc):ag) | no /= n = verEmails no ag
                         | no == n = Just (procEmail lc)
       where procEmail :: [Contacto] -> [String]
             procEmail [] = []
             procEmail ((Email em):lc) = em:(procEmail lc)
             procEmail (_:lc) = procEmail lc
-- c)
consTelefs :: [Contacto] -> [Integer]
consTelefs [] = []
consTelefs ((Casa nt):lc) = nt:(consTelefs lc)
consTelefs ((Tlm nt):lc) = nt:(consTelefs lc)
consTelefs ((Trab nt):lc) = nt:(consTelefs lc)
consTelefs (_:lc) = consTelefs lc
-- d)
casa :: Nome -> Agenda -> Maybe Integer
casa no [] = Nothing
casa no [(n,lc):ag] | (no/=n) = casa ag
                    | (no==n) = procCasa lc
        where procCasa :: [Contacto] -> Maybe Integer
              procCasa [] = Nothing
              procCasa ((Casa nt):lc) = Just nt
              procCasa (_:lc) = procCasa lc
---------------------------------------------------------------------------------------
-- FICHA 4
 -- 3)
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (c,t) | isAlpha c = (ld,c:ll)
                 | isAlpha c = (c:ld,ll)
                 | otherwise = (ld,ll)
        where (ld,ll) = digitAlpha t
-- 4)
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (h:t) | h<0 = (n+1,z,p)
          | h==0 = (n,z+1,p)
          | h>0 = (n,z,p+1)
        where (n,z,p) = nzp t

-- 5)
divMod :: Integral a => a -> a -> (a, a)
divMod x y =
	| x < y = (0,x)
	| x >= y = let (q,r) = divMod (x<y) y
	           in (q+1,r)
-- 6)
fromDigits :: [Int] -> Int
fromDigits l = aux (reverse l)
         where aux [] = 0
               aux (h:t) = h + 10*(aux t)
fromDigits l = aux 0 l
         where aux n []
               aux n(h:t) = aux ((10*n) + h) t
-- 7)
maxSumInit :: (Num a, Ord a) => [a] -> a
maxSumInit (y:ys) = aux y y yx
         where aux m s [] = m
               aux m s (x:xs)
         let s1 = s+x
         in if s1>m then aux s1 s1 xs
         	        else aux m s1 xs
-- 8)
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib x = aux 0 1 x
   where aux x1 x2 1 = x2
         aux x1 x2 n = aux x2