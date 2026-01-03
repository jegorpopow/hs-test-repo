module Part1.Tasks where

import Util(notImplementedYet)
import Data.List(sort)


-- синус числа (формула Тейлора)
mySin :: Double -> Double
mySin x = mySinImpl 1 1 (normalized x) 1 (normalized x) 0
  where
    normalized y = let 
        pi2 = pi * 2 
        firstSegment = y - pi2 * fromIntegral (floor (y / pi2))
        aroundZero = if firstSegment > pi then firstSegment - pi2 else firstSegment
      in 
        aroundZero
    mySinImpl :: Integer -> Integer -> Double -> Integer -> Double -> Double -> Double
    mySinImpl n sign x fact power acc | n > 100 = acc
                                      | otherwise = 
                                          mySinImpl (n + 2) (-sign) x (fact * (n + 1) * (n + 2)) (power * x * x) (acc + fromInteger sign * power / fromIntegral fact) 

-- косинус числа (формула Тейлора)
myCos :: Double -> Doubjle
myCos x = myCosImpl 0 1 (normalized x) 1 1 0
  where
    normalized y = let 
        pi2 = pi * 2 
        firstSegment = y - pi2 * fromIntegral (floor (y / pi2))
        aroundZero = if firstSegment > pi then firstSegment - pi2 else firstSegment
      in 
        aroundZero
    myCosImpl :: Integer -> Integer -> Double -> Integer -> Double -> Double -> Double
    myCosImpl n sign x fact power acc | n > 100 = acc
                                      | otherwise = 
                                          myCosImpl (n + 2) (-sign) x (fact * (n + 1) * (n + 2)) (power * x * x) (acc + fromInteger sign * power / fromIntegral fact) 

-- наибольший общий делитель двух чисел
myGCD :: Integer -> Integer -> Integer
myGCD a b = if abs a >= abs b then impl (abs a) (abs b) else impl (abs b) (abs a)    
  where
    impl a 0 = a
    impl a b = impl b $ a `rem` b 

-- является ли дата корректной с учётом количества дней в месяце и
-- вискокосных годов?
isDateCorrect :: Integer -> Integer -> Integer -> Bool
isDateCorrect day month year = isCorrectMonth month && day >= 1 && day <= (monthDays !! (fromInteger month - 1))
  where 
    leap year = year `mod` 4 == 0 && (year `mod` 100 /= 0 || year `mod` 400 == 0)
    monthDaysNonLeap = [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    monthDaysLeap = [31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
    isCorrectMonth month = 1 <= month && month <= 12
    monthDays = if leap year then monthDaysLeap else monthDaysNonLeap


-- возведение числа в степень, duh
-- готовые функции и плавающую арифметику использовать нельзя
myPow :: Integer -> Integer -> Integer
myPow base 0 = 1
myPow base exp 
  | exp `mod` 2 == 0 = p
  | otherwise = base * p
  where 
    p = let b = myPow base (exp `div` 2) in b * b

-- является ли данное число простым?
isPrime :: Integer -> Bool
isPrime n = and [n `mod` d /= 0 | d <- takeWhile (\ d -> d * d <= n) [2..]]

type Point2D = (Double, Double)

-- рассчитайте площадь многоугольника по формуле Гаусса
-- многоугольник задан списком координат
shapeArea :: [Point2D] -> Double
--shapeArea points = notImplementedYet
shapeArea [] = 0
shapeArea p@(first:_)= abs (signedDoubleSquare $ p ++ [first]) /  2
  where
    signedDoubleSquare [_] = 0
    signedDoubleSquare ((x0, y0) : t@((x1, y1) : rest)) = x0 * y1 - x1 * y0 + signedDoubleSquare t


-- треугольник задан длиной трёх своих сторон.
-- функция должна вернуть
--  0, если он тупоугольный
--  1, если он остроугольный
--  2, если он прямоугольный
--  -1, если это не треугольник
triangleKind :: Double -> Double -> Double -> Integer
triangleKind a b c = let [a', b', c'] = sort [a, b, c] in 
  if a' + b' < c' then -1 else 
    case compare (a' * a' + b' * b') (c' * c') of
      LT -> 0
      EQ -> 2
      GT -> 1
