-- FICHA 6

data BTree a = Empty
             | Node a (BTree a) (BTree a)
             deriving Show
-- 2a)
minimo :: Ord a => BTree a -> a
minimo (Node x Empty _) = x
minimo (Node x e d) = minimo e

-- b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x e d) = (x,d)
semMinimo (Node x e d) = 
	let (m,e1) = semMinimo e
	in (m, Node x e1 d)

-- d)
remove :: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node y e d)
    | x<y = (Node y (remove x e) d)
    | x>y = (Node y e (remove x d))
    | e==Empty = d
    | d==Empty = e
    | otherwise = let (m,d1) = semMinimo
                  in Node m e d1

-- 3)
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
                   deriving Show
type Turma = BTree Aluno

-- a)
inscNum :: Numero -> Turma -> Bool
inscNum num Empty = False
inscNum num (Node (n,_,_,_) e d)
   | num==n = True
   | num<n = inscNum num e
   | num>n = inscNum num d

-- c)
trabEst :: Turma -> [(Numero,Nome)]
trabEst Empty = []
trabEst (Node (num,Nome,TE,_) e d) = (trabEst e) ++ ([num,Nome] ++ (trabEst d))
trabEst (Node _ e d) = (trabEst e) ++ (trabEst d)

-- d)
nota :: Numero -> Turma -> Maybe Classificacao
nota _ Empty = Nothing
nota n (Node (num,_,_,e) e d)
   | n==num = just e
   | n<num = nota n e
   | n>num = nota n d