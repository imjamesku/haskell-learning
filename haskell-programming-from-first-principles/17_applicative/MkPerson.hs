validateLength :: Int -> String -> Maybe String
validateLength maxLen s =
  if length s > maxLen
    then Nothing
    else Just s

newtype Name = Name String deriving (Show)

newtype Address = Address String deriving (Show)

mkName :: String -> Maybe Name
mkName s = fmap Name $ validateLength 25 s

mkAddress :: String -> Maybe Address
mkAddress a = fmap Address $ validateLength 100 a

data Person = Person Name Address deriving (Show)

mkPerson :: String -> String -> Maybe Person
mkPerson n a = Person <$> mkName n <*> mkAddress a

mkPerson' :: String -> String -> Maybe Person
mkPerson' n a = do
  n' <- mkName n
  a' <- mkAddress a
  return $ Person n' a'