import Data.Monoid
newtype DList a = DL {
                     unDL::[a]->[a]
                     }

empty::DList a
empty = DL id

class Foo f where
      i :: f a a
      link :: f b c -> f a b -> f a c

newtype Tuple a b = Tuple (a, b)


instance Foo Tuple where
      i = getTuple
      link = dot

unTuple :: Tuple a b -> (a, b)
unTuple (Tuple (a, b)) = (a, b)

dot :: Tuple b c -> Tuple a b -> Tuple a c           
t1 `dot` t2 = Tuple (a, c)
              where
                a = fst . unTuple $ t2
                c = snd . unTuple $ t1

getTuple :: Tuple a a
-- getTuple = Tuple (,) 