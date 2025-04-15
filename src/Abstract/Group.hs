module Abstract.Group where
import Data.Monoid (Sum, Product (Product, getProduct))

infixl 7 ~~

class Monoid m => Group m where
    invert :: m -> m

    -- | Group subtraction
    (~~) :: m -> m -> m
    x ~~ y = x <> invert y

    -- | Repeated group operation application
    pow :: (Integral x) => m -> x -> m
    a0 `pow` n0 = case compare n0 0 of
        LT -> invert . f a0 $ negate n0
        EQ -> mempty
        GT -> f a0 n0
        where
            f a n
                | n == 1 = a
                | even n = f (a <> a) (n `quot` 2)
                | otherwise = g (a <> a) (n `quot` 2) a
            g a n c
                | n == 1 = a <> c
                | even n = g (a <> a) (n `quot` 2) c
                | otherwise = g (a <> a) (n `quot` 2) (a <> c)

instance Group () where
    invert () = ()

instance Num a => Group (Sum a) where
    invert = negate

instance Fractional a => Group (Product a) where
    invert = Product . recip . getProduct
