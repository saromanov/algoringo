
import Control.Monad

data Ring a = Ring a deriving Show

instance Functor Ring where
	fmap f (Ring num) = Ring (f num)

plus::Ring Int -> Int -> Ring Int
plus (Ring num) value = Ring (num + value)

product:: Ring Int -> Int -> Ring Int
product (Ring num) value = Ring (num * value)