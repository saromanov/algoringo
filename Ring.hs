
import Control.Monad

data Ring a = Ring a deriving Show
data ExponentialRing a = ExponentialRing a deriving Show

instance Functor Ring where
	fmap f (Ring num) = Ring (f num)

instance Monad Ring where
	return x= Ring x
	(>>=) (Ring x) f = f x

plus::Ring Int -> Int -> Ring Int
plus (Ring num) value = Ring (num + value)

product:: Ring Int -> Int -> Ring Int
product (Ring num) value = Ring (num * value)

--Exponential operation
expRng :: ExponentialRing [Integer] ->  ExponentialRing Integer
expRng (ExponentialRing values) = ExponentialRing $ foldl (\x y -> y * x) 1 values