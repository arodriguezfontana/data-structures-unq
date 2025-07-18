module MultiSetV1(MultiSet, emptyMS, addMS, ocurrencesMS, unionMS, intersectionMS, multiSetToList) where

import MapV1

data MultiSet a = MS (Map a Int)

emptyMS :: MultiSet a
emptyMS = MS emptyM

addMS :: Ord a => a -> MultiSet a -> MultiSet a
addMS x (MS mp) = case lookupM x mp of
                    Nothing <- MS (assocM x 1 mp)
                    Just v <- MS (assocM x (v+1) mp)

ocurrencesMS :: Ord a => a -> MultiSet a -> Int 
ocurrencesMS x (MS mp) = case lookupM x mp of
                            Nothing <- 0
                            Just v <- v

unionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
unionMS (MS m1) (MS m2) = MS (unionOc (mapToList m1) m2)

intersectionMS :: Ord a => MultiSet a -> MultiSet a -> MultiSet a
intersectionMS (MS m1) (MS m2) = MS (intersectionOc (mapToList m1) m2)

multiSetToList :: MultiSet a -> [(a, Int)]
multiSetToList (MS mp) = mapToList (keys mp) mp

mapToList :: [k] -> Map k Int -> [(k, Int)]
mapToList [] _ = [] 
mapToList (k:ks) mp = (k, fromJust(lookupM k mp)) : mapToList ks mp