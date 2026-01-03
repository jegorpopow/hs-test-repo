module Part5.Tasks where

import Util(notImplementedYet)

-- Реализуйте левую свёртку
myFoldl :: (b -> a -> b) -> b -> [a] -> b
myFoldl _ i [] = i 
myFoldl f i (h : t) = myFoldl f (f i h) t 

-- Реализуйте правую свёртку
myFoldr :: (a -> b -> b) -> b -> [a] -> b
myFoldr _ i [] = i
myFoldr f i (h : t) = f h $ myFoldr f i t

-- Используя реализации свёрток выше, реализуйте все остальные функции в данном файле

myMap :: (a -> b) -> [a] -> [b]
myMap f = myFoldr (\ a acc -> f a : acc) [] 

myConcatMap :: (a -> [b]) -> [a] -> [b]
myConcatMap f = foldr (\ a acc -> foldr (:) acc $ f a) [] 

myConcat :: [[a]] -> [a]
myConcat = myConcatMap id 

myReverse :: [a] -> [a]
myReverse = myFoldl (\ acc a -> a : acc) []

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter p = myFoldr (\ a acc -> if p a then a : acc else acc) [] 

myPartition :: (a -> Bool) -> [a] -> ([a], [a])
myPartition p = myFoldr (\ a (l, r) -> if p a then (a : l, r) else (l, a : r))  ([], [])  

