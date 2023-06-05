module Ficha3 where
-- 3 (d)
data Contacto = Casa Integer
                | Trab Integer
                | Tlm Integer
                | Email String
                deriving Show
type Nome = String
type Agenda = [(Nome, [Contacto])]

casa::Nome->Agenda->Maybe Integer 
casa n ((x,y):t) | n==x = daCasa y 
                 | otherwise = casa n t 
casa n [] = Nothing 

daCasa:: [Contacto]->Maybe Integer
daCasa ((Casa n):t) = Just n 
daCasa (h:t) = daCasa t 
daCasa [] = Nothing 