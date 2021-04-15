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