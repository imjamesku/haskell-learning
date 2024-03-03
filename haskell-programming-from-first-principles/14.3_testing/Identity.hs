module Identity where

import Test.QuickCheck

data Identity a = Identity a deriving (Eq, Show)

instance (Arbitrary a) => Arbitrary (Identity a) where
  arbitrary = identityGen

identityGen :: (Arbitrary a) => Gen (Identity a)
identityGen = do
  a <- arbitrary
  return (Identity a)
