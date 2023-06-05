module Ficha5 where
import Data.ByteString (sort)
import Ficha2 (Polinomio)

-- 1(a)
myAny:: (a->Bool) -> [a] -> Bool 
myAny p [] = False
myAny p (x:xs) = if p x
                 then True 
                 else myAny p xs 
-- 1(g)
sortOn:: Ord b => (a->b) -> [a] -> [a]
sortOn f [] = []
sortOn f (x:xs) = insert f x (sortOn f xs)
insert:: Ord b => (a->b) -> a -> [a] -> [a]
insert f x [] = [x]
insert f x (y:ys) | (f x)<(f y) = x:y:ys
                  | otherwise= y:insert f x ys 
-- 2(a)
selgrau:: Int-> Polinomio -> Polinomio
selgrau n p = filter (\(c,e)->e==n) p
-- 2(d)
deriv:: Polinomio -> Polinomio
deriv pol = map aux pol
          where aux (a,b) = (a*(fromIntegral b),b-1)
-- 3 (h)
type Mat a = [[a]]
rotateLeft:: Mat a -> Mat a 
rotateLeft [l] = map (:[]) (reverse l)
rotateLeft (l:ls) = zipWith (:) (reverse l) (rotateLeft ls)
-------OU--------------
rotateLeft1:: Mat a -> Mat a 
rotateLeft1 ([]:_) = []
rotateLeft1 m = map last m : rotateLeft1 (map init m)