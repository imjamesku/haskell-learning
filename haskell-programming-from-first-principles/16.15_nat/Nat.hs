-- nat :: (f -> g) -> f a -> g a
-- nat = undefined

type Nat f g = forall a. f a -> g a

maybeToList :: Nat Maybe []
maybeToList Nothing = []
maybeToList (Just a) = [a]

maybeToList' :: Maybe a -> [a]
maybeToList' Nothing = []
maybeToList' (Just a) = [a]