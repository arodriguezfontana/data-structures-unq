module StackV1(Stack, emptyS, isEmptyS, push, top, pop, lenS) where

data Stack a = S [a] Int
-- Inv. Rep.
-- Sea (S xs c) una pila, xs los elementos de la misma y c la cantidad de elementos.
-- * c es la cantidad de elementos de xs.

emptyS :: Stack a -- O(1)
-- Crea una pila vacía.
emptyS = S [] 0

isEmptyS :: Stack a -> Bool -- O(1)
-- Dada una pila indica si está vacía.
isEmptyS (S xs _) = null xs

push :: a -> Stack a -> Stack a -- O(1)
-- Dados un elemento y una pila, agrega el elemento a la pila.
push x (S xs c) = S (x:xs) (c+1)

top :: Stack a -> a -- O(1)
-- Dada un pila devuelve el elemento del tope de la pila.
top (S xs c) = head xs

pop :: Stack a -> Stack a -- O(1)
-- Dada una pila devuelve la pila sin el primer elemento.
pop (S xs c) = S (tail xs) (c-1)

lenS :: Stack a -> Int -- O(1)
-- Dada la cantidad de elementos en la pila.
lenS (S _ c) = c