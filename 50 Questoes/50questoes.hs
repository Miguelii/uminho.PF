-- 1) -> V

enumFromTo1 :: Int -> Int -> [Int]
enumFromTo1 x y | x == y = [x]
                | x < y = x:enumFromTo1 (x+1) y
                | otherwise = []
                
-- 2) DUVIDA

enumFromThenToo :: Int -> Int -> Int -> [Int] 
enumFromThenToo x y z = if (x <= z) then (x:(enumFromThenToo y (y+(y-1)) z))
                                    else []
-- 3) -> V

(+++) :: [a] -> [a] -> [a]
(+++) [] l = l
(+++) (h:t) l = h:( (+++) t l)  

-- 4) -> V

(!!!) :: [a] -> Int -> a
(!!!) (h:t) 0 = h
(!!!) (h:t) x = (!!!) t (x-1)

-- 5) -> V

reverse1 :: [a] -> [a]
reverse1 [] = []
reverse1 (h:t) = reverse t ++ [h]

-- 6) -> V

take1 :: Int -> [a] -> [a]
take1 0 l = []
take1 x [] = []
take1 x (h:t) | x>0 = (h:take1(x-1) t)

-- 7) -> V

drop1 :: Int -> [a] -> [a]
drop1 0 l = l
drop1 x [] = []
drop1 x (h:t) | x>0 = drop1(x-1) t

-- 8) -> V

zip1 :: [a] -> [b] -> [(a,b)]
zip1 [] l2 = []
zip1 l1 [] = []
zip1 (x:xs) (y:ys) = (x,y):zip1 xs ys

-- 9) -> V

elem1 ::  Eq a => a -> [a] -> Bool
elem1 x [] = False
elem1 x (h:t) = if (x==h) then True
                          else (elem1 x t)

-- 10) -> V

replicate1 :: Int -> a -> [a]
replicate1 0 x = []
replicate1 n x = x:replicate1 (n-1) x

-- 11) -> V

intersperse1 :: a -> [a] -> [a]
intersperse1 x [] = []
intersperse1 x [h] = [h]
intersperse1 x (h:t) = h:x:intersperse1 x t

-- 12) DUVIDA

group1 :: Eq a => [a] -> [[a]]
group1 [] = []
group1 [x] = [[x]]
group1 (x:y:xs) = let (first:rest) = group1 (y:xs)
                  in if (x /= y) then [x]:first:rest
                               else (x:first):rest

group2 :: Eq a => [a] -> [[a]]
group2 [] = []
group2 (h:t) = (h:takeWhile (== h) t) : group2 (dropWhile (== h) t)


-- 13) -> V

concat1 :: [[a]] -> [a]
concat1 [] = []
concat1 (h:t) = h ++ concat1 t

-- 14) -> V

inits1 :: [a] -> [[a]]
inits1 [] = []
inits1 l = inits1 ( init l) ++ [l] -- Init já esta pré defenida, retoma uma lista sem o seu ultimo elemento

-- 15) -> V

tails1 :: [a] -> [[a]]
tails1 [] = []
tails1 l = l: tails1 (tail l) -- Tail já pre defenida, drop do primeiro elemento da lista

-- 16) -> V

isPrefixOff :: Eq a => [a] -> [a] -> Bool
isPrefixOff [] a = True
isPrefixOff a [] = False
isPrefixOff (x:xs) (y:ys) = x==y && isPrefixOff xs ys -- Nao precisamos de if pois o && já tem valor boleano
                           
-- 17) -> V

isSuffixOff :: Eq a => [a] -> [a] -> Bool
isSuffixOff [] a = True
isSuffixOff a [] = False
isSuffixOff l (x:xs) = l == (x:xs) || isSuffixOff l xs -- Nao precisamos de if pois o || já tem valor boleano

-- 18) -> V

isSubsequenceOff :: Eq a => [a] -> [a] -> Bool
isSubsequenceOff [] a = True
isSubsequenceOff a [] = False
isSubsequenceOff (x:xs) (y:ys) = x==y && isSubsequenceOff xs ys || isSubsequenceOff (x:xs) ys

-- 19)

elemIndices1 :: Eq a => a -> [a] -> [Int]
elemIndices1 a [] = []
elemIndices1 x (h:t) | x==h = 0:map (+1) (elemIndices1 x t)
                     | otherwise = map (+1) (elemIndices1 x t)
                     
-- 20) -> V

nub1 :: Eq a => [a] -> [a]
nub1 [] = []
nub1 (h:t) = if ( elem h t) then nub1 t
             else h:nub1 t


-- 21) -> V

delete1 :: Eq a => a -> [a] -> [a] 
delete1 x [] = []
delete1 x (h:t) = if (x==h) then t
                  else h:delete1 x t

-- 22) -> V

remove1 :: Eq a => [a] -> [a] -> [a] 
remove1 l [] = l
remove1 [] l = []
remove1 l (h:t) = remove1 (delete1 h l) t

-- 23) -> V

union1 :: Eq a => [a] -> [a]-> [a]
union1 l [] = l 
union1 l (h:t) = if ( elem h l ) then (union1 l t)
                else  union1 (l ++ [h]) t

-- 24) -> V

intersect1 :: Eq a => [a] -> [a] -> [a]
intersect1 [] l = []
intersect1 (h:t) l = if ( elem h l) then ( h:intersect1 t l)
                      else (intersect1 t l)

-- 25) -> V

insert1 :: Ord a => a -> [a] -> [a] 
insert1 x [] = [x]
insert1 x (h:t) = if ( x<=h ) then ( x:h:t )
                  else ( h:insert1 x t)

-- 26) -> 

unwords1 :: [String] -> String
unwords1 [] = ""
unwords1 (h:t) = h ++ (if t==[] then "" else " ") ++ unwords1 t

-- 27) -> V

unlines1 :: [String] -> String
unlines1 [] = ""
unlines1 (h:t) = h ++ "\n" ++ unlines1 t

-- 28) DUVIDA

pMaior1 :: Ord a => [a] -> Int
pMaior1 [a] = 0
pMaior1 (h:t) = let x = pMaior1 t
                in if ( h > (t !! x)) then 0
                else 1+x

-- 29) -> V

temRepetidos1 :: Eq a => [a] -> Bool
temRepetidos1 [] = False
temRepetidos1 (h:t) = elem h t || temRepetidos1 t

-- 30) -> V

algarismos1 :: [Char] -> [Char] 
algarismos1 [] = []
algarismos1 (h:t) = if ( elem h ['0'..'9']) then h:algarismos1 t
                    else algarismos1 t

-- 31) -> V

posImpares1 :: [a] -> [a]
posImpares1 [] = []
posImpares1 [a] = []
posImpares1 (h:s:t) = s:posImpares1 t

-- 32) -> V

posPares1 :: [a] -> [a]
posPares1 [] = []
posPares1 [a] = [a]
posPares1 (h:s:t) = h:posPares1 t

-- 33) -> V

isSorted1 :: Ord a => [a] -> Bool
isSorted1 [] = True
isSorted1 [a] = True
isSorted1 (h:s:t) = s>=h && isSorted1 (s:t)

-- 34) -> V

iSort1 :: Ord a => [a] -> [a]
iSort1 [] = []
iSort1 (h:t) = insert1 h (iSort1 t)

-- 35) -> V

menor1 :: String -> String -> Bool
menor1 l "" = False
menor1 "" l = True
menor1 (x:xs) (y:ys) = x<y || menor1 xs ys

-- 36)

elemMSet1 :: Eq a => a -> [(a,Int)] -> Bool
elemMSet1 a [] = False
elemMSet1 a ((x,n):t) = a==x || elemMSet1 a t 

-- 37)

lengthMSet1 :: [(a,Int)] -> Int 
lengthMSet1 [] = 0
lengthMSet1 ((x,n):xs) = n + lengthMSet1 xs

-- 38)

converteMSet1 :: [(a,Int)] -> [a]
converteMSet1 [] = []
converteMSet1 ((x,1):xs) = x:converteMSet1 xs
converteMSet1 ((x,n):xs) = x:converteMSet1 ((x,n-1):xs)

-- 39)

insereMSet1 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet1 x [] = [(x,1)]
insereMSet1 x ((a,n):xs) = if (x==a) then ((a,n+1):xs)
                           else (a,n):insereMSet1 x xs

-- 40)

removeMSet1 :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet1 x [] = []
removeMSet1 x ((a,n):xs) = if (x==a) then xs
                           else (a,n):removeMSet1 x xs

-- 41)

constroiMSet1 :: Ord a => [a] -> [(a,Int)]
constroiMSet1 [] = []
constroiMSet1 (l:ls) = insereMSet1 l (constroiMSet1 ls)

-- 42)

partitionEithers1 ::  [Either a b] -> ([a],[b])
partitionEithers1 lista = (as lista, bs lista)
  where
    as ((Left x):xs)  = x : as xs
    as ((Right x):xs) = as xs
    as otherwise      = []
    bs ((Right x):xs) = x : bs xs
    bs ((Left x):xs)  = bs xs
    bs otherwise      = []


-- 43)

catMaybes1 :: [Maybe a] -> [a]
catMaybes1 [] = []
catMaybes1 ((Just a):xs) = a:catMaybes1 xs
catMaybes1 (Nothing:xs) = catMaybes1 xs

-- 44)

data Movimento = Norte | Sul | Este | Oeste deriving Show

posicao :: (Int,Int) -> [Movimento] -> (Int,Int)
posicao p [] = p
posicao (x,y) (l:ls) = posicao ( case l of Norte -> (x,y+1)
                                           Sul -> (x,y-1)
                                           Este -> (x+1,y)
                                           Oeste -> (x-1,y)) ls

-- 45)

caminho :: (Int,Int) -> (Int,Int) -> [Movimento]
caminho (xi,yi) (xf,yf)
    | xi < xf = Este:caminho (xi + 1, yi) (xf, yf)
    | xi > xf = Oeste:caminho (xi - 1, yi) (xf, yf)
    | yi < yf = Norte:caminho (xi, yi + 1) (xf, yf)
    | yi > yf = Sul:caminho (xi, yi - 1) (xf, yf)
    | otherwise = []

-- 46)
vertical :: [Movimento] -> Bool
vertical [] = True
vertical (l:ls) = case l of Este -> False
                            Oeste -> False
                            otherwise -> vertical ls

--47)
data Posicao = Pos Int Int deriving Show

maisCentral :: [Posicao] -> Posicao
maisCentral [(Pos x y)] = (Pos x y)
maisCentral ((Pos x y):(Pos a b):ps) = if (x^2 + y^2) < (a^2 + b^2) then maisCentral ((Pos x y):ps) 
                                       else maisCentral ((Pos a b):ps)

-- 48)

vizinhos ::  Posicao -> [Posicao] -> [Posicao]
vizinhos (Pos x y) ps = filter (\(Pos a b) -> (abs (a - x) + abs (b - y) == 1)) ps 

-- 49)

mesmaOrdenada :: [Posicao] -> Bool
mesmaOrdenada [(Pos x y)] = True
mesmaOrdenada ((Pos x y):(Pos x2 y2):ps) = y == y2 && mesmaOrdenada ((Pos x2 y2):ps)

-- 50)

data Semaforo = Verde | Amarelo | Vermelho deriving Show

interseccaoOK :: [Semaforo] -> Bool
interseccaoOK ss = length [s | s <- ss, case s of Vermelho -> False; otherwise -> True] < 2
