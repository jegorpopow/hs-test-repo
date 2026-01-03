module Part4.Tasks where

import Util(notImplementedYet)

-- Перевёрнутый связный список -- хранит ссылку не на последующию, а на предыдущую ячейку
data ReverseList a = REmpty | (ReverseList a) :< a
infixl 5 :<

-- Функция-пример, делает из перевёрнутого списка обычный список
-- Использовать rlistToList в реализации классов запрещено =)
rlistToList :: ReverseList a -> [a]
rlistToList lst =
    reverse (reversed lst)
    where reversed REmpty = []
          reversed (init :< last) = last : reversed init

-- Реализуйте обратное преобразование
listToRlist :: [a] -> ReverseList a
listToRlist lst = foldr (flip (:<)) REmpty $ reverse lst

-- Реализуйте все представленные ниже классы (см. тесты)
instance Show a => Show (ReverseList a) where
    showsPrec _ REmpty = ("[]" ++ ) 
    showsPrec _ rlist@(r :< t) = ('[' :) . showPrecImpl rlist . (']' : ) 
      where
        showPrecImpl (REmpty :< t) = (show t ++)
        showPrecImpl (r :< t)  = showPrecImpl r . (',' : ) . (show t ++)
    show a = showsPrec 0 a ""

instance Eq a => Eq (ReverseList a) where
    (==) (r :< t) (r' :< t') = t == t' && r == r'
    (==) REmpty REmpty = True
    (==) _ _ = False
    (/=) a b = not $ a == b 

instance Semigroup (ReverseList a) where
    (<>) a REmpty = a
    (<>) a (r :< t) = (a <> r) :< t 

instance Monoid (ReverseList a) where
    mappend = (<>)
    mempty = REmpty

instance Functor ReverseList where
    fmap f REmpty = REmpty 
    fmap f (r :< t) = fmap f r :< f t

instance Applicative ReverseList where
    pure x = (REmpty :< x)
    (<*>) (r :< t) args =  (r <*> args) <> fmap t args
    (<*>) REmpty _ = REmpty

instance Monad ReverseList where
    return = pure
    REmpty >>= k = REmpty
    (r :< t) >>= k = (r >>= k) <> k t 
