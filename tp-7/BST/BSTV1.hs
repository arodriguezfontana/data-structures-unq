data Tree a = NodeT a (Tree a) (Tree a) | EmptyT deriving Show

belongsBST :: Ord a => a -> Tree a -> Bool
belongsBST _ EmptyT = False
belongsBST x (NodeT x2 ti td) = x==x2 || x < x2 && belongsBST x ti || belongsBST x td

insertBST :: Ord a => a -> Tree a -> Tree a
insertBST x EmptyT = NodeT x EmptyT EmptyT
insertBST x (NodeT x2 ti td) = if x==x2  then NodeT x ti td else
                               if x < x2 then NodeT x2 (insertBST x ti) td
                                         else NodeT x2 ti (insertBST x td)

deleteBST :: Ord a => a -> Tree a -> Tree a
deleteBST _ EmptyT = EmptyT
deleteBST x (NodeT x2 ti td) = if x==x2  then remakeBST ti td else
                               if x < x2 then NodeT x2 (deleteBST x ti) td
                                         else NodeT x2 ti (deleteBST x td)

remakeBST :: Ord a => Tree a -> Tree a -> Tree a
remakeBST EmptyT EmptyT = EmptyT
remakeBST ti     EmptyT = ti
remakeBST ti     td     = let (min,td2) = splitMinBST td in NodeT min ti  td2

splitMinBST :: Ord a => Tree a -> (a, Tree a)
splitMinBST EmptyT              = error "There is no minimum"
splitMinBST (NodeT x EmptyT td) = (x,td)
splitMinBST (NodeT x ti     td) = let (min,ti2) = splitMinBST ti
                                   in (min, NodeT x ti2 td)

splitMaxBST :: Ord a => Tree a -> (a, Tree a)
splitMaxBST EmptyT              = error "There is no maximum"
splitMaxBST (NodeT x ti EmptyT) = (x,ti)
splitMaxBST (NodeT x ti     td) = let (max,td2) = splitMaxBST td
                                   in (max, NodeT x ti td2)

esBST :: Ord a => Tree a -> Bool
esBST EmptyT = True
esBST (NodeT x EmptyT EmptyT) = True
esBST (NodeT x ti     EmptyT) = x > root ti && esBST ti
esBST (NodeT x EmptyT td) = x < root td && esBST td
esBST (NodeT x ti     td) = x > root ti && x < root td && esBST ti && esBST td

elMaximoMenorA :: Ord a => a -> Tree a -> Maybe a
elMaximoMenorA _ EmptyT                   = Nothing
elMaximoMenorA x (NodeT x2 EmptyT EmptyT) = if x > x2 then Just x2 else Nothing
elMaximoMenorA x (NodeT x2 ti     td)     = if x<=x2  then elMaximoMenorA x ti

maxM :: Ord a => a -> Maybe a -> Maybe a
maxM x Nothing  = Just x
maxM x (Just y) = Just (max x y)

elMinimoMayorA :: Ord a => a -> Tree a -> Maybe a
elMinimoMayorA _ EmptyT = Nothing
elMinimoMayorA x (NodeT x2 EmptyT EmptyT) = if x < x2 then Just x2 else Nothing
elMinimoMayorA x (NodeT x2 ti     td) = if x>=x2  then elMinimoMayorA x td
                                                      else minM x2 (elMinimoMayorA x ti)
minM :: Ord a => a -> Maybe a -> Maybe a
minM x Nothing = Just x
minM x (Just y) = Just (min x y)

isBalanced :: Tree a -> Bool
isBalanced EmptyT = True
isBalanced (NodeT _ ti td) = abs (heightT ti - heightT td) <= 1
                             && isBalanced ti && isBalanced td