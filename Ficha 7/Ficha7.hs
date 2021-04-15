-- FICHA 7

-- 1)
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt

-- a)
calcula :: ExpInt -> Int
calcula (Const x) = x
calcula (Simetrico e) = (-1) + (calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2
calcula (Menos e1 e2) = calcula e1 - calcula e2
calcula (mult e1 e2) = calcula e1 * calcula e2

-- b)
infixa :: ExpInt -> String
infixa (Const x) = Show x
infixa (Simetrico e) = "(" ++ (infixa e1) ++ "+" ++
                       (infixa e2) ++ ")"

-- 2)
data RTree a = R a [RTree a]

-- a)
soma :: Num a => RTree a -> a
soma (R x []) = x
soma (R x l) =x + sum (map soma l)

-- b)
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1+(maxinum (map altura l))

-- d)
mirror :: RTree a -> RTree a
mirror (R x l) = R x (reverse (map mirror l))

-- e)
postorder :: RTree a -> [a]
postorder (R x l) = concat(map postorder l) ++ [x]

-- 3)
data BTree a = Empty | Node a (BTree a) (BTree a)
data LTree a = Tip a | Fork (LTree a) (LTree a)

-- a)
ltSum :: Num a => LTree a -> a 
ltSum (Tip x) = x
ltSum (Fork e d) = ltSum e ++ ltSum d

-- b)
listaLT :: LTree a -> [a]
listaLT (Tip x) = [x]
listaLT (Fork e d) = (listaLT e) ++ (listaLT d)

-- c)
ltHeight :: LTree a -> Int
ltHeight (Tip _ ) = 1
ltHeight (Fork e d) = 1+ (max (ltHeight e)(ltHeight d))

-- 4)
data FTree a b = Leaf b | No a (FTree a b) (FTree a b)

-- a)
splitFTree :: FTree a b -> (BTree a, LTree b)
splitFTree (Leaf x) = (Empty,Tip x)
splitFTree (No y e d) = let (bte,lte) = split e
                            (btd,ltd) = split d
                        in (Node y bte btd,Fork lte ltd)

-- b)
joinTrees :: BTree a -> LTree b -> Maybe (FTree a b)
joinTrees _ (Tip x) = Just (leaf x)
joinTrees Empty _ = Nothing
joinTrees (Node y e d)(Fork e1 d1) = let fte = joinTrees e e1
                                         ftd = joinTrees d d1
                                     in case fte of
                                     	Nothing -> Nothing
                                     	(Just te) -> cade ftd of
                                     		Nothing -> Nothing
                                     		(Just td) -> Just(Node y te td)