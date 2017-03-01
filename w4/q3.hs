data Maybe3 x = Value x | Novalue | Null

instance (Eq x) => Eq (Maybe3 x) where
 Value x == Value y = x == y
 Novalue == Novalue = True
 Null /= Null = True
 _ == _ = False