module Util where

mapTuple :: (a -> b) -> (a, a) -> (b, b)
mapTuple f (a1, a2) = (f a1 , f a2 )

mapTupleSep :: (a -> t) -> (l -> q) -> (a,l)  -> (t,q)
mapTupleSep f1 f2 (a,l) = ((f1 a ),(f2 l))


updtFst :: (a -> t) -> (a,b) -> (t,b)
updtFst f (a,b) = (f a, b)


updtSnd :: (b -> t) -> (a,b) -> (a,t)
updtSnd f (a,b) = (a,f b)

listA_minus_B :: Eq a => [a] -> [a] -> [a]
listA_minus_B (xs) (y:ys) = if(elem y xs) then (listA_minus_B xs ys) else (y:((listA_minus_B xs ys)))
listA_minus_B xs []      = []


replaceElement :: Int -> a -> [a] -> [a]
replaceElement n iten [] = []
replaceElement n item ls = a ++ (item:b) where (a, (_:b)) = splitAt n ls



editElement :: Eq a => (a -> a)  -> a ->[a] -> [a]
editElement f n (x:xs) = if(n == x) then (f x : xs) else editElement f n xs
editElement f n [] = []

maybeOf :: a -> (a -> Bool) -> Maybe a
maybeOf a f = if (f a) then (Just a) else Nothing

removeElem :: (a->Bool) -> [a] -> [a]
removeElem f (a:as) = if (f a) then removeElem f as else (a: (removeElem f as))
removeElem f [] = []


----- Application specific ---

