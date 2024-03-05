data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
  fmap f m = case m of
    First a -> First a
    Second b -> Second (f b)

instance Applicative (Sum a) where
  pure = Second

  (<*>) f x = case (f, x) of
    (First a, _) -> First a
    (_, First a) -> First a
    (Second f', Second x') -> Second (f' x')

instance Monad (Sum a) where
  return = pure

  (>>=) m f = case m of
    First a -> First a
    Second b -> f b