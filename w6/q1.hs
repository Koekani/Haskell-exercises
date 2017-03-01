data Maybe' a = Null | Novalue | Value a deriving (Show)

instance (Eq m) => Eq (Maybe' m) where
  Value x == Value y = x == y
  Novalue == Novalue = True
  _ == _ = False

instance Functor Maybe' where
  fmap f (Value x) = Value (f x)  
  fmap f Novalue = Novalue
  fmap f Null = Null

instance Applicative Maybe' where
   pure = Value
   Novalue <*> _ = Novalue
   Null <*> _ = Null
   (Value f) <*> something = fmap f something 


instance Monad Maybe' where
   return x = Value x
   Novalue >>= f = Novalue
   Null >>= f = Null
   Value x >>= f = f x
   fail _ = Novalue
