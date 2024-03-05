data Validation err a = Failure err | Success a deriving (Eq, Show)

data Errors = DividedByZero | StackOverflow | MooglesChewedWires deriving (Eq, Show)

-- success = Success (+ 1) <*> 1 :: Validation [Errors] Int

instance Functor (Validation e) where
  fmap f (Success a) = Success (f a)
  fmap _ (Failure e) = Failure e

instance (Monoid e) => Applicative (Validation e) where
  pure = Success
  (<*>) (Success f) (Success a) = Success (f a)
  (<*>) (Failure e) (Success _) = Failure e
  (<*>) (Success _) (Failure e) = Failure e
  (<*>) (Failure e1) (Failure e2) = Failure (e1 `mappend` e2)