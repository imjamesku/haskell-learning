newtype Identity a = Identity {runIdentity :: a} deriving (Eq, Ord, Show)

newtype IdentityT f a = IdentityT {runIdentityT :: f a} deriving (Eq, Ord, Show)

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity x) = Identity (f x)

instance (Functor m) => Functor (IdentityT m) where
  fmap :: (a -> b) -> IdentityT m a -> IdentityT m b
  fmap f (IdentityT fa) = IdentityT (fmap f fa)

instance Applicative Identity where
  pure = Identity
  (<*>) :: Identity (a -> b) -> Identity a -> Identity b
  (<*>) (Identity f) (Identity a) = Identity $ f a

instance (Applicative m) => Applicative (IdentityT m) where
  pure :: a -> IdentityT m a
  pure = IdentityT . pure
  (<*>) :: IdentityT m (a -> b) -> IdentityT m a -> IdentityT m b
  (<*>) (IdentityT fab) (IdentityT fa) = IdentityT (fab <*> fa)

instance Monad Identity where
  return = pure
  (Identity a) >>= f = f a

instance (Monad m) => Monad (IdentityT m) where
  return = pure
  (IdentityT ma) >>= f = IdentityT $ ma >>= runIdentityT . f