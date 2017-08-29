import Control.Applicative

-- state monad
type State = Int
newtype ST a = S (State -> (a, State))

app :: ST a -> State -> (a, State)
app (S st) x = st x

instance Functor ST where
-- fmap :: (a -> b) -> ST a -> ST b
fmap g (S st) = S (\s -> let (x, s') = st s in (g x, s'))

instance Applicative ST where
pure x = \s -> (x, s)
stf <*> stx = S (\s -> let (f, s') = app stf s
                           (x, s'') = app stx s' in (f x, s''))

--instance Monad ST where
instance Monad ST where
-- (>>=) :: ST a -> (a -> ST b) -> ST b
stx >>= f = S(\s -> let (x, s') = app stx s in app (f x) s')


