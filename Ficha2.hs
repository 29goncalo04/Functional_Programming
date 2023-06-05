module Ficha2 where
import Data.Char
-- 2(a)
dobros:: [Float]->[Float]
dobros (h:t) = 2*h:dobros t
dobros []=[]
-- 2(b)
numOcorre:: Char->String->Int
numOcorre x (h:t) = if x==h
                    then 1+numOcorre x t 
                    else numOcorre x t
numOcorre x [] = 0
-- 2(c)
positivos:: [Int]->Bool
positivos (h:t) | h>0 && positivos t = True
                | otherwise = False 
positivos [] = True 
-- 2(d)
soPos:: [Int]->[Int]
soPos [] = []
soPos (h:t) = if h>0
              then h:soPos t
              else soPos t
-- 2(e)
somaNeg:: [Int]->Int
somaNeg (h:t) | h<0 = h+somaNeg t 
              | otherwise = somaNeg t
somaNeg [] = 0
-- 2(f)
tresUlt:: [a]->[a]
tresUlt l = if length l <=3
            then l 
            else tresUlt (tail l) 
-- 2(g)
segundos:: [(a,b)]->[b]
segundos (h:t)= snd h: segundos t
segundos []= []
-- 2(h)
nosPrimeiros:: (Eq a)=>a->[(a,b)]->Bool
nosPrimeiros a [] = False
nosPrimeiros a ((x,y):t) = if a==x 
                           then True 
                           else nosPrimeiros a t
-- 2(i)
sumTriplos:: (Num a, Num b, Num c)=>[(a,b,c)]->(a,b,c)
sumTriplos ((x,y,z):(a,b,c):t) = sumTriplos((x+a,y+b,z+c):t) 
sumTriplos ((x,y,z):[]) = (x,y,z)
-- 3(a)
soDigitos:: [Char]->[Char]
soDigitos (h:t)= if isDigit h
                 then h:soDigitos t
                 else soDigitos t
soDigitos []= []
-- 3(b)
minusculas:: [Char]->Int
minusculas [] = 0
minusculas (h:t) = if isLower h 
                   then 1+minusculas t 
                   else minusculas t 
-- 3(c) 
nums:: String->[Int]
nums (h:t) = if isDigit h 
             then digitToInt h:nums t 
             else nums t 
nums [] = []
-- 4(a)
type Polinomio = [Monomio]
type Monomio = (Float,Int)
conta:: Int->Polinomio->Int 
conta n (h:t)= if snd h == n 
               then 1+conta n t 
               else conta n t 
conta n []=0
-- 4(b)
grau::Polinomio->Int
grau[(a,b)]=b 
grau ((a,b):(c,d):t) |b>d= max b (grau ((a,b):t))
                     |b<d= max d (grau ((c,d):t)) 
                     |otherwise= max b (grau ((a,b):t))
-- 4(c)
selgrau:: Int->Polinomio->Polinomio
selgrau n (h:t) = if snd h == n 
                  then h:selgrau n t 
                  else selgrau n t 
selgrau n []=[]
-- 4(d)
deriv:: Polinomio->Polinomio
deriv (h:t) = (fst h *fromIntegral (snd h), snd h -1):deriv t 
deriv []=[]
-- 4(e) 
calcula:: Float->Polinomio->Float 
calcula a (h:t)= ((a^snd h)*fst h) + calcula a t 
calcula a []=0
-- 4(f)
simp:: Polinomio->Polinomio
simp (h:t) = if fst h /=0
             then h:simp t 
             else simp t 
simp []=[]
-- 4(g)
mult:: Monomio->Polinomio->Polinomio
mult (a,b) (h:t)= (a*fst h,b+snd h):mult (a,b) t
mult (a,b) []=[]
-- 4(h)
normaliza:: Polinomio->Polinomio 
normaliza ((a,b):(c,d):t) |b==d = normaliza ((a+c,b):t)
                          |conta b t == 0 = (a,b):normaliza ((c,d):t)
                          |otherwise = normaliza ((a,b):t ++ [(c,d)]) 
normaliza [(a,b)]= [(a,b)]
normaliza []=[]
-- 4(i)
soma:: Polinomio->Polinomio->Polinomio
soma p1 p2 = normaliza (p1++p2)
-- ???????????????
-- 4(j)
produto:: Polinomio->Polinomio->Polinomio
produto (h:t) (a:b)= normaliza (mult h(a:b) ++ produto t(a:b))
produto (h:t) []=[]
produto [] (h:t)=[]
produto [] []=[]
-- 4(k)
ordena:: Polinomio->Polinomio
ordena [] = []
ordena ((c,e):t) = insere (c,e) (ordena t)

insere:: Monomio->Polinomio->Polinomio
insere (c,e) [] = [(c,e)]
insere (c,e) ((a,b):t) | e<b = (c,e):(a,b):t 
                       | e==b && c<=a = (c,e):(a,b):t
                       | e==b && c>a = (a,b):(c,e):t 
                       | otherwise= (a,b) : insere (c,e) t 
-- 4(l)
equiv::Polinomio->Polinomio->Bool
equiv p1 p2 = ordena (normaliza p1) == ordena (normaliza p2)