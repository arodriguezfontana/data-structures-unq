import StackV1

apilar :: [a] -> Stack a
apilar [] = emptyS
apilar (x:xs) = push x (apilar xs)

desapilar :: Stack a -> [a]
desapilar s = if isEmptyS s 
                then []
                else top s : desapilar (pop s)

insertarEnPos :: Int -> a -> Stack a -> Stack a
insertarEnPos 0 x s = push x s
insertarEnPos n x s = push (top s) (insertarEnPos (n-1) x (pop s))