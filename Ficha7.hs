module Ficha7 where 
data ExpInt = Const Int
            | Simetrico ExpInt
            | Mais ExpInt ExpInt
            | Menos ExpInt ExpInt
            | Mult ExpInt ExpInt
   deriving Show 
exp1 = Mais (Const 3) (Menos (Const 2) (Const 5))
--1(a)
calcula:: ExpInt -> Int 
calcula (Const x) = x 
calcula (Simetrico e) = -(calcula e)
calcula (Mais e1 e2) = calcula e1 + calcula e2 
calcula (Menos e1 e2) = calcula e1 - calcula e2 
calcula (Mult e1 e2) = calcula e1 * calcula e2 
--1(b)
infixa :: ExpInt -> String
infixa (Const x) = show x 
infixa (Simetrico e) = "-("++ infixa e ++")"
infixa (Mais e1 e2) = "(" ++ infixa e1 ++ " + " ++ infixa e2
infixa (Menos e1 e2) = "(" ++ infixa e1 ++ " - " ++ infixa e2
infixa (Mult e1 e2) = "(" ++ infixa e1 ++ " * " ++ infixa e2
--1(c)
posfixa :: ExpInt -> String
posfixa (Const x) = show x
posfixa (Simetrico e) = posfixa e ++ " " ++ " ~"
posfixa (Mais e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " +"
posfixa (Menos e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " -"
posfixa (Mult e1 e2) = posfixa e1 ++ " " ++ posfixa e2 ++ " *"
--2
data RTree a = R a [RTree a]
 deriving Show
rose2 = R 9 [R 1 [], R 3 []]
rose3 = R 8 [R 6 []]
rose1 = R 7 [R 2 [], rose2, rose3]
--2(a)
soma :: Num a => RTree a -> a
soma (R x l) = x + sum (map soma l)
--2(b)
altura :: RTree a -> Int
altura (R x []) = 1
altura (R x l) = 1 + maximum (map altura l)
--2(c)
prune :: Int -> RTree a -> RTree a
prune 1 (R x l) = R x []
prune n (R x l) = R x (map (prune (n-1)) l)