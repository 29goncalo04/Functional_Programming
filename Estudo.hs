module Estudo where 
import Data.ByteString (intersperse)
import Data.List (isSubsequenceOf)
-- somatório com acumulador
ac:: Num a => a->[a]->a 
ac x [] = x 
ac x (h:t) = ac (x+h) t

somatorio:: Num a => [a] -> a 
somatorio l = ac 0 l 

myzip:: [a]->[b]->[(a,b)]
myzip l [] = []
myzip [] l = []
myzip (x:xs) (h:t) = (x,h):myzip xs t 

preCrescente':: Ord a=> [a]->[a]
--preCrescente' [] = []
preCrescente' [x] = [x]
preCrescente' (h:t) | h>=(head t) = [h]
                    | otherwise = h:preCrescente' t 

amplitude:: [Int]->Int 
amplitude [] = 0
amplitude [x] = x 
amplitude l = mymax l - mymin l 

mymax:: Ord a=>[a]->a 
--mymax [] = 0 
mymax [x] = x 
mymax (h:t) |h<=head t = mymax t 
            |otherwise = mymax (h:tail t)
mymin:: Ord a=>[a]->a 
--mymin [] = 0 
mymin [x] = x 
mymin (h:t) |h>=head t = mymin t 
            |otherwise = mymin (h:tail t)  


type Mat a = [[a]]
soma:: Num a => Mat a -> Mat a -> Mat a 
soma [] [] = []
soma ((x:b):c) ((d:e):f) = (somalistas (x:b) (d:e)) : soma c f 
somalistas::Num a => [a]->[a]->[a]
somalistas l [] = l 
somalistas [] l = l 
somalistas (x:b) (c:d) = (x+c):somalistas b d 

func:: [[Int]]->[Int]
func [] = []
func (a:b) | sum a >10 = a ++ (func b)
           | otherwise = (func b)

(\\\)::Eq a=> [a]->[a]->[a]
(\\\) [] l=[] 
(\\\) l [] = l 
(\\\) (h:t) (x:xs) | elem h (x:xs) = (\\\) t (remove h (x:xs))                             
                   | otherwise = h:(\\\) t (x:xs) 
remove::Eq a=>a->[a]->[a]
remove n [] = []
remove n (h:t) | n==h = t                    
               | otherwise = h:remove n t 

replicate'::Int->a->[a]
replicate' n x | n<=0 = []
               | otherwise= x:replicate' (n-1) x 

intersperse'::a->[a]->[a]
intersperse' x [] = []
intersperse' x [n] = [n]
intersperse' x (h:t) = h:x:intersperse' x t

concat' ::[[a]]->[a]
concat' [] = []
concat' (h:t) = h++concat' t 

inits'::[a]->[[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l] 

tails'::[a]->[[a]]
tails' [] = [[]]
tails' l = [l]++tails' (tail l)

heads':: [[a]]->[a]
heads' ([]:t) = heads' t 
heads' [] = []
heads' (h:t) = (head h):heads' t 

total'::[[a]]->Int
total' ([]:t) = total' t 
total' [] = 0
total' (h:t) = length h + total' t 

fun'::[(a,b,c)]->[(a,c)]
fun' []=[]
fun' ((x,y,z):t) = (x,z):fun' t 

cola'::[(String,b,c)]->String
cola' [] = []
cola' ((x,y,z):t) = x++cola' t

idade'::Int->Int->[(String,Int)]->[String]
idade' _ _ [] = []
idade' x n ((s,i):t) | x-i<n = idade' x n t 
                     | otherwise = s:idade' x n t 

isPrefixOf'::Eq a=>[a]->[a]->Bool 
isPrefixOf' [] l = True 
isPrefixOf' l [] = False
isPrefixOf' (h:t) (x:xs) | h==x = isPrefixOf' t xs 
                         | otherwise = False   

isSuffixOf'::Eq a=>[a]->[a]->Bool 
isSuffixOf' [] l = True 
isSuffixOf' l [] = False
isSuffixOf' h x | last h==last x = isSuffixOf' (init h) (init x) 
                | otherwise = False         

isSubsequenceOf'::Eq a=>[a]->[a]->Bool
isSubsequenceOf' [] [] = True
isSubsequenceOf' l [] = False
isSubsequenceOf' [] l = True
isSubsequenceOf' (h:t) (x:xs) | h==x = isSubsequenceOf' t xs 
                              | otherwise = isSubsequenceOf' (h:t) xs      

elemIndices'::Eq a =>a->[a]->[Int]
elemIndices' x [] = []
elemIndices' x l = help 0 x l 
help::Eq a=>Int->a->[a]->[Int]
help k x [] = []
help k x (h:t) | x==h = k:help (k+1) x t 
               | otherwise= help (k+1) x t

nub'::Eq a=>[a]->[a]
nub' [] = []
nub' (h:t) | elem h t = nub' t 
           | otherwise= h:nub' t 