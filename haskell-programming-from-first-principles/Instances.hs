module Instances where

newtype Identity a = Identity a

instance Functor Identity where
  fmap :: (a -> b) -> Identity a -> Identity b
  fmap f (Identity x) = Identity (f x)

data Pair a = Pair a a

instance Functor Pair where
  fmap :: (a -> b) -> Pair a -> Pair b
  fmap f (Pair x1 x2) = Pair (f x1) (f x2)

data Three a b c = Three a b c

instance Functor (Three a b) where
  fmap :: (a2 -> b2) -> Three a1 b1 a2 -> Three a1 b1 b2
  fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b

instance Functor (Three' a) where
  fmap f (Three' a1 b1 b2) = Three' a1 (f b1) (f b2)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
  fmap f (Four' a1 a2 a3 b) = Four' a1 a2 a3 (f b)

data Trivial = Trivial

-- instance Functor Trivial