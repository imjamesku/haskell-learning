type Nat f g a = f a -> g a

maybeToList :: Nat Maybe [] a
maybeToList Nothing = []
maybeToList (Just a) = [a]

degenerateMtl :: (Num a) => Nat Maybe [] a
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a + 1]