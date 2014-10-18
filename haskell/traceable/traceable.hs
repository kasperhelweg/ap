import Control.Applicative

newtype Trace a = T (a, String)
                deriving Show

instance Functor Trace where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (T (v, s)) = T (f v, s)

instance Applicative Trace where
  -- pure :: a -> f a
  pure v = T (v, "")

  -- (<*>) :: f (a -> b) -> f a -> f b
  T (f, _) <*> T (v, s) = T (f v, s)

instance Monad Trace where
  -- (>>=) :: Trace a -> (a -> Trace b) -> Trace b
  (T p) >>= f = let (x, s) = p
                    T (fx, fs) = f x
                in  T (fx, s ++ fs)
    
  -- return :: a -> Trace a
  return x = T (x, "")

newtype MV a = MV [a]
             deriving Show

instance Functor MV where
  -- fmap :: (a -> b) -> f a -> f b
  fmap f (MV p) = MV $ map f p 

instance Applicative MV where
  -- pure :: a -> f a
  pure v = MV [v]

  -- (<*>) :: f (a -> b) -> f a -> f b
  MV [f] <*> MV p = MV $ map f p

instance Monad MV where
  -- (>>=) :: MV a -> (a -> MV b) -> MV b
  (MV p) >>= f = let rs = map f p
                 in MV $ concat $ map (\(MV x) -> x) rs
  
  -- return :: a -> Multivalued a
  return x = MV [x]






traceable :: String -> (t -> a) -> t -> Trace a
traceable name f = \x -> T (f x, name ++ " called.")

ft = traceable "ft" (\x -> x + 2)
gt = traceable "gt" (\x -> x + 4)

m = do y <- ft 4
       z <- gt y
       return z
