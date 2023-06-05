enumFromThenTo':: Int->Int->Int->[Int]
enumFromThenTo' x y z | x>=z || x>=y = []
                      | otherwise = x:enumFromThenTo' y (y+(y-x)) z
reverse':: [a]->[a]
reverse' [] = []
reverse' (h:t) = (reverse' t) ++ [h]

take':: Int->[a]->[a]
take' n []=[]
take' 0 (h:t)=[]
take' n (h:t) = h:take' (n-1) t 

replicate':: Int->a->[a]
replicate' n x | n<=0 = []
               | otherwise = x:replicate' (n-1) x 

inits':: [a]->[[a]]
inits' [] = [[]]
inits' l = inits' (init l) ++ [l]

idade:: Int->Int->[(String,Int)]->[String]
idade a b [] = []
idade a b (h:t) | a- snd h >= b = fst h:idade a b t 
                | otherwise = idade a b t

powerEnumFrom:: Int->Int->[Int]
powerEnumFrom n m = help 0 n m 
help:: Int->Int->Int->[Int]
help k n m | k == m-1 = [n^k]
           | k>m-1 = []
           | otherwise = (n^k):help (k+1) n m 

issuffixof':: Eq a=>[a]->[a]->Bool 
issuffixof' [] t = True
issuffixof' t [] = False
issuffixof' x y = last x == last y && issuffixof' (init x) (init y)

(\\\):: Eq a => [a] -> [a]-> [a]
(\\\) l [] = l 
(\\\) [] l = [] 
(\\\) (a:b) (x:y) | a==x = (\\\) b y 
                  | otherwise = a: (\\\) b (x:y)