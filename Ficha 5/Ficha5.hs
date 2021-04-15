-- FICHA 5

-- 1)
--a)
any :: (a -> Bool) -> [a] -> Bool
any p [] = False
any p (x:xs) | p x = True
             | not p(x) = any p xs

-- b)
zipWith :: (a->b->c) -> [a] -> [b] -> [c]
zipWith _ [] _ = []
zipWith _ _ [] = []

zipWith g (x:xs)(y:ys) = (g x y):(zipWith g xs ys)
-- c)
takeWhile :: (a->Bool) -> [a] -> [a]
takeWhile p [] = []
takeWhile p (x:xs) | p s = x:(takeWhile p x)
                   | otherwise = []

-- d)
dropWhile :: (a->Bool) -> [a] -> [a]
dropWhile p [] = []
dropWhile p (x:xs) | p x = dropWhile p xs
                   | otherwise = (x:xs)

-- e)
span :: (a-> Bool) -> [a] -> ([a],[a])
span p l = aux p ([],[]) l
    where aux p (l,s) [] = (l,s)
          aux p (l,s) (x:xs)
                 | p s = aux p (l++ [x], x|xs)
                 | otherwise = (l,(x:xs))

-- g)
sortOn :: Ord b => (a -> b) -> [a] -> [a]
sortOn _ [] = []
sortOn p (x:xs) =
     let l = filter (\y -> (p y) < (p x) ):xs
         r = filter (\y -> (p y) >= (p x) ):xs
     in (sortOn p l) ++ [x] ++ (sortOn p r)
-- OU
sortOn1 :: Ord b => (a -> b) -> [a] -> [a]
sortOn1 p (x:xs) = ordenaP p x (sortOn1 p xs)
     where ordenaP p x [] = [x]
           ordenaP p x (y:ys)
              | (p x) < (p y) = x:y:ys
              | otherwise = y:(ordenaP p x ys)

-- 2)
type Polinomio = [Monomio]
type Monomio = (Float,Int)

-- a)
selgrau :: Int -> Polinomio -> Polinomio
selgrau e p = filter (\(c1,e1) -> e1==e2 ) p

-- b)
conta :: Int -> Polinomio -> Int
conta e p = length( filter (\(c1,e1) -> (e1==e)) p )

-- d)
deriv :: Polinomio -> Polinomio
deriv p = map (\(c,e) -> (c*e,e-1)) p

-- e)
calcula :: Float -> Polinomio -> Float
calcula e p = sum ( map(\(c1,e1) -> c*(e^e1)) p )

-- f)
simp :: Polinomio -> Polinomio
simp p = filter (\(c1,e1) -> c1/=0) p

-- 3)
type Mat a = [[a]]

-- a)
dimOK :: Mat a -> Bool
dimOK [l] = True
dimOK (l1:l2:m) | (length l1) /= (length l2) = False
                | otherwise = dimOK (l2:m)

-- b)
dimMat :: Mat a -> (Int,Int)
dimMat m = (length m, length (head m))

-- g)
triSup :: Num a => Mat a -> Bool
triSup m = aux 0 m
   where aux _ [] = True
         aux i (l:m) = all (==0)(take i l) && (aux (i+1) m )
         
-- h)
rotateLeft :: Mat a -> Mat a
rotateLeft ([]:m) = m
rotateLeft m = (map last m):(rotateLeft (map init m))