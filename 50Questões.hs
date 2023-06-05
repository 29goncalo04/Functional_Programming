module CinquentaQuestões where
import Data.List
-- PERGUNTA 1    
numfrom:: Int->Int->[Int]
numfrom x y | x<=y = x:numfrom (x+1) y
            | otherwise = []
-- PERGUNTA 2
numfromthento:: Int->Int->Int->[Int]
numfromthento x y z | x>=y || y>=z && (y-x)>z || x>=z = []
                    | otherwise = x:numfromthento y (y+(y-x)) z
-- PERGUNTA 3
(+++) :: [a]->[a]->[a]
(+++) (h:t) []=(h:t)
(+++) (h:t) (a:b) = h:(+++)t(a:b)
-- PERGUNTA 4 
(!!!):: [a]->Int->a 
(!!!) (h:t) x | x==0 = h
              | otherwise= (!!!) t (x-1)  
-- PERGUNTA 5
reverse2:: [a]->[a]
reverse2 []=[]
reverse2 (h:t)= reverse2 t ++ [h]
-- PERGUNTA 6
takee:: Int->[a]->[a]
takee x []=[]
takee 0 (h:t)=[]
takee x (h:t) | x>length (h:t) = (h:t)
              | x<length (h:t) = h:takee (x-1) t 
              | x==length (h:t) = (h:t)
-- PERGUNTA 7 
drop2:: Int->[a]->[a]
drop2 n []=[]
drop2 0 l=l
drop2 n (a:h:t) = if n>= length (a:h:t)
                 then []
                 else drop2 n (h:t)
-- PERGUNTA 8
zipp:: [a]->[b]->[(a,b)]
zipp (h:t) (i:j) = (h,i):zipp t j 
zipp [] [] = []
zipp l [] = []
zipp [] l = []
-- PERGUNTA 9
replicatee:: Int->a->[a]
replicatee 0 x = []
replicatee n x = x:replicate (n-1) x
-- PERGUNTA 10
interspersee:: a->[a]->[a]
interspersee x (h:t) |length (h:t)>1 = h:x:interspersee x t 
                     |length (h:t) == 1 = [h]
                     |otherwise = []
interspersee x [] = []
-- PERGUNTA 11

-- PERGUNTA 12
concat':: [[a]]->[a] 
concat' [] = []
concat' (x:xs) = x++ concat' xs
-- PERGUNTA 13
myinits:: [a]->[[a]]
myinits [] = [[]]
myinits xs = myinits (init xs) ++ [xs]
-- PERGUNTA 14
mytails:: [a]->[[a]]
mytails [] = [[]] 
mytails (h:t) = (h:t) : mytails t
-- PERGUNTA 15
myheads:: [[a]]->[a]
myheads [] = []
myheads ([]:l) = myheads l
myheads (a:b) = head a: myheads b
-- PERGUNTA 16 
mytotal:: [[a]]->Int
mytotal [] = 0
mytotal ([]:b) = mytotal b
mytotal (a:b) = length a + mytotal b
-- PERGUNTA 17
myfun:: [(a,b,c)]->[(a,c)]
myfun [] = []
myfun ((x,y,z):t) = (x,z): myfun t
-- PERGUNTA 18
mycola:: [(String,b,c)]->String 
mycola [] = ""
mycola ((a,b,c):t) = a++mycola t 
-- PERGUNTA 19
myidade:: Int->Int->[(String,Int)]->[String]
myidade x y ((s,i):t) | x-i<y = myidade x y t 
                      | otherwise = s : myidade x y t 
myidade x y [] = []
-- PERGUNTA 20
powerenumfrom:: Int->Int->[Int]
powerenumfrom n 1 = [1]
powerenumfrom n m = oi 0 n m 

oi:: Int->Int->Int->[Int]
oi k n m | m==1 = [1]
         | k==(m-1) = [n^k]
         | otherwise = n^k:oi (k+1) n m
-- PERGUNTA 21
isprime :: Int -> Bool
isprime x | mod x n == 0 = n == n-1
          | mod x 2 == 0 = False
          |otherwise = True
          where n = x-1  
-- PERGUNTA 22
isprefixof:: Eq a => [a]->[a]->Bool
isprefixof [] (x:y) = True  
isprefixof (x:y) [] = False
isprefixof (h:t) (x:y) | h==x && isprefixof t y = True 
                       | otherwise = False
-- PERGUNTA 23
issuffixof:: Eq a=>[a]->[a]->Bool 
issuffixof [] t = True
issuffixof t [] = False
issuffixof x y | last x == last y && issuffixof a b = True
               | otherwise = False
               where a = init x 
                     b = init y
-- PERGUNTA 24
issubsequenceof:: Eq a=>[a]->[a]->Bool
issubsequenceof [] _ = True
issubsequenceof _ [] = False 
issubsequenceof (x:y) (a:b) | x==a && issubsequenceof y b = True 
                            | otherwise = issubsequenceof (x:y) b 
-- PERGUNTA 25 
elemindices:: Eq a=>a->[a]->[Int]
elemindices n [] = []
elemindices n (x:xs) = elemindices2 0 n (x:xs)

elemindices2:: Eq a=>Int->a->[a]->[Int]
elemindices2 k n [] = []
elemindices2 k n (x:xs) | n==x = k:elemindices2 (k+1) n xs 
                        | otherwise = elemindices2 (k+1) n xs
-- PERGUNTA 26
nub' :: Eq a => [a] -> [a]
nub' [] = []
nub' (x:xs) | elem x xs = nub' xs 
            | otherwise = x:nub' xs 
-- PERGUNTA 27 
delete' :: Eq a => a -> [a]-> [a]
delete' n [] = []
delete' n (x:xs) | n==x = xs
                 | otherwise = x: delete' n xs
-- PERGUNTA 28 
(\\\):: Eq a => [a] -> [a]-> [a]
(\\\) l [] = l 
(\\\) [] l = [] 
(\\\) l (x:y) = (\\\) (delete x l) y 
-- PERGUNTA 29 
union' :: Eq a => [a] -> [a] -> [a]
union' [] l = l
union' l [] = l
union' l (x:y) | elem x l = union' l y 
               | otherwise = union' (l ++ [x]) y 
-- PERGUNTA 30 
intersect' :: Eq a => [a] -> [a] -> [a]
intersect' [] l = []
intersect' l [] = []
intersect' (a:b) (x:y) | elem a (x:y) = a:intersect' b (x:y)
                       | otherwise = intersect' b (x:y)
-- PERGUNTA 31 
insert' :: Ord a => a -> [a] -> [a]
insert' n [] = [n]
insert' n (a:b) |n<=a = n:a:b
                |otherwise = a:insert' n b 
-- PERGUNTA 32 
unwords':: [String]->String 
unwords' [] = ""
unwords' (a:b) = a ++ " " ++ unwords' b
-- PERGUNTA 33 
unlines' :: [String] -> String
unlines' [] = ""
unlines' (x:y) = x ++ "\n" ++ unlines' y 
-- PERGUNTA 34 

-- PERGUNTA 35 
lookup' :: Eq a => a -> [(a,b)] -> Maybe b
lookup' a [] = Nothing 
lookup' a ((x,y):t) | a==x = Just y
                    | otherwise = lookup' a t
-- PERGUNTA 36 
preCrescente' :: Ord a => [a] -> [a]
preCrescente' [] = []
preCrescente' [h] = [h]
preCrescente' (x:y) | x>head y = [x]
                    | otherwise = x:preCrescente' y
-- PERGUNTA 37 
iSort' :: Ord a => [a] -> [a]
iSort' [] = []
iSort' (h:t) = insert h (iSort' t) 
-- PERGUNTA 38 
menor' :: String -> String -> Bool
menor' [] _ = True 
menor' _ [] = False 
menor' (a:b) (x:y) | a==x = menor' b y  
                   | otherwise = a<x 
-- PERGUNTA 39
elemMSet' :: Eq a => a -> [(a,Int)] -> Bool
elemMSet' a [] = False 
elemMSet' a ((b,n):t) | a==b = True
                      | otherwise = elemMSet' a t 
-- PERGUNTA 40 
converteMSet' :: [(a,Int)] -> [a]
converteMSet' [] = []
converteMSet' ((a,n):t) | n == 1 = a:converteMSet' t 
                        | otherwise = a:converteMSet' ((a,n-1):t)

-- PERGUNTA 41 
insereMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
insereMSet' a [] = [(a,1)]
insereMSet' a ((x,y):t) | a==x = (x,y+1):t
                        | otherwise = (x,y):insereMSet' a t 
-- PERGUNTA 42 
removeMSet' :: Eq a => a -> [(a,Int)] -> [(a,Int)]
removeMSet' a [] = []
removeMSet' a ((x,y):t) | a==x && y>1 = (x,y-1):t
                        | a==x && y==1 = t 
                        | otherwise = (x,y):removeMSet' a t 
-- PERGUNTA 43
constroiMSet' :: Ord a => [a] -> [(a,Int)]
constroiMSet' [] = []
constroiMSet' (h:t) = insereMSet' h (constroiMSet' t)
-- PERGUNTA 44

-- PERGUNTA 45 
catMaybes':: [Maybe a]->[a]
catMaybes' [] = []
catMaybes' (Nothing:t) = catMaybes' t 
catMaybes' ((Just a):t) = a:catMaybes' t  
-- PERGUNTA 46 
data Movimento = Norte | Sul | Este | Oeste
               deriving Show 
caminho:: (Int,Int)->(Int,Int)->[Movimento]
caminho (a,b) (c,d) | (a,b) == (c,d) = []
                    | a<c = Este:caminho (a+1,b) (c,d)
                    | a>c = Oeste:caminho (a-1,b) (c,d)
                    | b<d = Norte:caminho (a,b+1) (c,d)
                    | b>d = Sul:caminho (a,b-1) (c,d)
-- PERGUNTA 47 

-- PERGUNTA 48
type Ponto = (Float,Float)
data Rectangulo = Rect Ponto Ponto
contaQuadrados :: [Rectangulo] -> Int
contaQuadrados [] = 0
contaQuadrados (h:t) | equadrado h = 1+ (contaQuadrados t)
                     | otherwise = contaQuadrados t 
equadrado::Rectangulo->Bool 
equadrado (Rect (a,b) (c,d)) = abs (c-a) == abs (d-b)
-- PERGUNTA 49
areaTotal :: [Rectangulo] -> Float
areaTotal [] = 0
areaTotal (Rect (a,b) (c,d):t) = ((abs(c-a))*(abs(d-b))) + areaTotal t
--PERGUNTA 50 
data Equipamento = Bom | Razoavel | Avariado
                 deriving Show
naoReparar :: [Equipamento] -> Int
naoReparar [] = 0 
naoReparar (Bom:t) = 1+naoReparar t 
naoReparar (Razoavel:t) = 1+naoReparar t 
naoReparar (Avariado:t) = naoReparar t  