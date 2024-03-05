newtype Constant a b = Constant {getConstant :: a} deriving (Eq, Ord, Show)

instance Functor (Constant a) where
  fmap :: (b -> c) -> Constant a b -> Constant a c
  fmap _ (Constant x) = Constant x

instance (Monoid a) => Applicative (Constant a) where
  pure _ = Constant mempty
  (<*>) (Constant f) (Constant x) = Constant x