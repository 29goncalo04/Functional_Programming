module Ficha4 where
import Data.Char 
-- 1
digitAlpha :: String -> (String,String)
digitAlpha [] = ([],[])
digitAlpha (x:xs) | isDigit x = (ls,x:ds)
                  | isAlpha x = (x:ls,ds)
                  | otherwise = digitAlpha xs
                  where (ls,ds) = digitAlpha  xs
-- OU 
digitAlphaAc:: String->(String,String)
digitAlphaAc s = digitAlphaAc' ([],[]) s
digitAlphaAc':: (String,String)->String->(String,String)
digitAlphaAc' (ls,ds) [] = (ls,ds)
digitAlphaAc' (ls,ds) (c:cs) | isDigit c = digitAlphaAc' (ls,c:ds) cs
                             | isAlpha c = digitAlphaAc' (c:ls,ds) cs
                             | otherwise = digitAlphaAc' (ls,ds) cs
-- 2 
nzp :: [Int] -> (Int,Int,Int)
nzp [] = (0,0,0)
nzp (x:xs) | x>0 = (a,b,1+c)
           | x==0 = (a,1+b,c)
           | x<0 = (1+a,b,c)
           where (a,b,c) = nzp xs 
-- 3
mydivMod :: Integral a => a -> a -> (a, a)
mydivMod x y | x==y = (1,0)
             | x<y = (0,x)
             | otherwise = (d+1,r)
             where (d,r) = mydivMod (x-y) y 