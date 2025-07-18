module StackV1(Stack, emptyS, isEmptyS, push, top, pop, lenS) where

data Stack a = S [a] Int
-- Inv. Rep.
-- Sea (S xs c) una pila, xs los elementos de la misma y c la cantidad de elementos.
-- * c es la cantidad de elementos de xs.

emptyS :: Stack a
emptyS = S [] 0

isEmptyS :: Stack a -> Bool
isEmptyS (S xs _) = null xs

push :: a -> Stack a -> Stack a
push x (S xs c) = S (x:xs) (c+1)

top :: Stack a -> a
top (S xs c) = head xs

pop :: Stack a -> Stack a
pop (S xs c) = S (tail xs) (c-1)

lenS :: Stack a -> Int
lenS (S _ c) = c