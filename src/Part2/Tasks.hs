module Part2.Tasks where

import Util(notImplementedYet)

data BinaryOp = Plus | Minus | Times deriving (Show, Eq)

data Term = IntConstant { intValue :: Int }          -- числовая константа
          | Variable    { varName :: String }        -- переменная
          | BinaryTerm  { op :: BinaryOp, lhv :: Term, rhv :: Term } -- бинарная операция
             deriving(Show,Eq)

-- Для бинарных операций необходима не только реализация, но и адекватные
-- ассоциативность и приоритет

infixl 6 |+|
infixl 6 |-|
infixl 7 |*|

(|+|) :: Term -> Term -> Term
(|+|) = BinaryTerm Plus
(|-|) :: Term -> Term -> Term
(|-|) = BinaryTerm Minus
(|*|) :: Term -> Term -> Term
(|*|) = BinaryTerm Times

-- Заменить переменную `varName` на `replacement`
-- во всём выражении `expression`
replaceVar :: String -> Term -> Term -> Term
replaceVar varName replacement i@IntConstant{intValue=intValue} = i
replaceVar varName replacement v@Variable{varName=varName'} 
  | varName == varName' = replacement
  | otherwise = v
replaceVar varName replacement BinaryTerm{op=op, lhv=lhv, rhv=rhv} = BinaryTerm op lhs' rhs'
  where 
    lhs' = (replaceVar varName replacement lhv)
    rhs' = (replaceVar varName replacement rhv)

-- Посчитать значение выражения `Term`
-- если оно состоит только из констант
evaluate :: Term -> Term
evaluate i@IntConstant{intValue=_} = i
evaluate v@Variable{varName=_} = v
evaluate b@BinaryTerm {op=op, lhv=lhv, rhv=rhv} = 
   case (evaluate lhv, evaluate rhv) of 
      (IntConstant{intValue=lhv'}, IntConstant{intValue=rhv'}) -> IntConstant $ apply op lhv' rhv'
      (_ , _) -> b
   where
    apply Plus = (+)
    apply Minus = (-)
    apply Times = (*) 
