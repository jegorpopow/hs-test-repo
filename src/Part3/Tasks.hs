module Part3.Tasks where

import Util (notImplementedYet)
import Data.List (group, sort, maximumBy)
import Data.Ord (comparing)
import Data.Char (digitToInt)

-- Функция finc принимает на вход функцию f и число n и возвращает список чисел [f(n), f(n + 1), ...]
finc :: (Int -> a) -> Int -> [a]
finc f n = fmap f [n..]

-- Функция ff принимает на вход функцию f и элемент x и возвращает список [x, f(x), f(f(x)), f(f(f(x))) ...]
ff :: (a -> a) -> a -> [a]
ff f x = x : (fmap f $ ff f x)

-- Дан список чисел. Вернуть самую часто встречающуюся *цифру* в этих числах (если таковых несколько -- вернуть любую)
mostFreq :: [Int] -> Int
mostFreq numbers = digitToInt . head . maximumBy (comparing length) . group . sort $ numbers >>= show

-- Дан список lst. Вернуть список элементов из lst без повторений, порядок может быть произвольным.
uniq :: (Eq a) => [a] -> [a]
uniq [] = []
uniq (h:t) = h : uniq (filter (/= h) t)

-- Функция grokBy принимает на вход список Lst и функцию F и каждому возможному
-- значению результата применения F к элементам Lst ставит в соответствие список элементов Lst,
-- приводящих к этому результату. Результат следует представить в виде списка пар.

grokBy :: (Eq k) => (a -> k) -> [a] -> [(k, [a])]
grokBy f lst = foldl insertGroup [] lst
  where
    insertGroup [] x = [(f x, [x])]
    insertGroup ((k, group):rest) x
      | f x == k   = (k, x:group) : rest
      | otherwise  = (k, group) : insertGroup rest x
