-- FICHA 3

-- 2)
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
