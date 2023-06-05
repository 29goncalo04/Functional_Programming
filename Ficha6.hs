-----------------10----------------
------3------------------18--------
----------7----------15------20----
---------------------------------25
data BTree a = Empty
            | Node a (BTree a) (BTree a)
            deriving Show
arv1 = Node 3 Empty (Node 7 Empty Empty)
arv2 = Node 20 Empty (Node 25 Empty Empty)
arv3 = Node 18 (Node 15 Empty Empty) arv2
arv4 = Node 10 arv1 arv3
--1(b)
contaNodos:: BTree a -> Int
contaNodos Empty = 0
contaNodos (Node x e d) = 1 + contaNodos e + contaNodos d
--1(a)
altura:: BTree a -> Int
altura Empty = 0
altura (Node x e d) = 1 + max (altura e) (altura d)
--1(c)
folhas:: BTree a -> Int
folhas (Node x Empty Empty) = 1
folhas Empty = 0
folhas (Node x e d) = folhas e + folhas d
--1(d)
prune:: Int -> BTree a -> BTree a 
prune 0 (Node x _ _) = Empty
prune n (Node x e d) = Node x (prune (n-1) e) (prune (n-1) d)
prune _ Empty = Empty
--1(e)
path :: [Bool] -> BTree a -> [a]
path _ Empty = []
path [] (Node x e d) = [x]
path (b:bs) (Node x e d) | b = x:path bs d 
                         | otherwise = x:path bs e 
--1(f)
mirror :: BTree a -> BTree a                         
mirror Empty = Empty
mirror (Node x e d) = Node x (mirror d) (mirror e)
--2(a)
minimo :: Ord a => BTree a -> a
minimo (Node x Empty d) = x 
minimo (Node x e d) = minimo e
--2(b)
semMinimo :: Ord a => BTree a -> BTree a
semMinimo (Node x Empty d) = d 
semMinimo (Node x e d) = Node x (semMinimo e) d 
--2(c)
minSmin :: Ord a => BTree a -> (a,BTree a)
minSmin (Node x Empty d) = (x,d)
minSmin (Node x e d) = (m,Node x e1 d)
                     where (m,e1) = minSmin e 
--2(d)
remove:: Ord a => a -> BTree a -> BTree a
remove x Empty = Empty
remove x (Node y e Empty) | x==y = e
remove x (Node y e d) | x<y = Node y (remove x e) d 
                      | x>y = Node y e (remove x d)
                      | x==y = Node m e d1
                             where (m,d1) = minSmin d
--3
type Aluno = (Numero,Nome,Regime,Classificacao)
type Numero = Int
type Nome = String
data Regime = ORD | TE | MEL deriving Show
data Classificacao = Aprov Int
                   | Rep
                   | Faltou
 deriving Show
type Turma = BTree Aluno

insereAluno:: Aluno -> Turma -> Turma 
insereAluno a@(n,x,r,c) (Node b@(n1,x1,r1,c1) e d) | n<n1 = Node (n1,x1,r1,c1) (insereAluno (n,x,r,c) e) d 
                                                   | n>n1 = Node b e (insereAluno a d)
                                                   | n==n1 = Node b e d 
insereAluno a Empty = Node a Empty Empty                                                    