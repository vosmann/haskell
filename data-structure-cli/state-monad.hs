import Control.Applicative

-- State monad 1
-- Defined in "Programming in Haskell", Graham Hutton

type IntState = Int

-- Constructor: takes a lambda and returns a ST.
newtype ST a = S (IntState -> (a, IntState))

-- Opposite of constructor: takes a lambda and returns an S() object.
app :: ST a -> IntState -> (a, IntState)
app (S st) x = st x

instance Functor ST where
    -- fmap :: (a -> b) -> ST a          -> ST b
    -- fmap :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))
    fmap g (S st) = S(\s -> let (x, s') = st s in (g x, s'))

instance Applicative ST where
    -- pure :: a -> ST a
    -- pure :: a -> (s -> (a, s))
    pure x = S(\s -> (x, s))

    -- (<*>) :: ST (a -> b)                 -> ST a          -> ST b
    -- (<*>) :: (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s))
    stf <*> stx = S(\s -> let (f, s') = app stf s
                              (x, s'') = app stx s' in (f x, s''))

instance Monad ST where
    return = pure
    -- (>>=) :: ST a -> (a -> ST b) -> ST b
    st >>= f = S(\s -> let (x, s') = app st s in app (f x) s')


-- State monad 2
-- Defined in "Control.Monad.State.Lazy"

-- Constructor: takes a lambda and returns a State.
-- In "State monad 1" this is the same, only here the lambda gets a name - runState.
newtype State s a = State { runState :: s -> (a,s) }
-- Unlike the ST type, the State type is generic for both the state type s and the emitted output type a.

-- Here are the State constructor function and the runState function.
-- They are isomorphic.
--
-- State    :: (s -> (a,s)) -> State s a
-- runState :: State s a -> (s -> (a,s))
-- runState :: State s a ->  s -> (a,s)
--
-- Like the app function in "State monad 1", the runState function merely
-- unpacks the lambda by removing the State constructor invocation.
-- However, runState does not apply the unpacked lambda.

-- app := runState + application of unpacked lambda

-- Five functions are implemented for the state monad:
-- get       :: State s s
-- put       :: s -> State s ()
-- modify    :: (s -> s) -> State s ()
-- evalState :: State s a -> s -> a
-- execState :: State s a -> s -> s

-- Before these five functions, we make State a functor, an applicative functor and a monad.

instance Functor (State s) where
    -- fmap :: (a -> b) -> State a       -> State b
    -- fmap :: (a -> b) -> (s -> (a, s)) -> (s -> (b, s))
    fmap g st = State (\s -> let (x, s') = runState st s in (g x, s'))

instance Applicative (State s) where
    -- pure :: a -> State a
    -- pure :: a -> (s -> (a, s))
    pure x = State (\s -> (x, s))

    -- (<*>) :: State (a -> b)       -> State a       -> State b
    -- (<*>) :: (s -> ((a -> b), s)) -> (s -> (a, s)) -> (s -> (b, s))
    stf <*> stx = State (\s -> let (f, s') = runState stf s
                                   (x, s'') = runState stx s' in (f x, s''))

instance Monad (State s) where
    return = pure
    -- (>>=) :: State a -> (a -> State b) -> State b
    st >>= f = State (\s -> let (x, s') = runState st s in runState (f x) s')


-- Emits the entire state.
get :: State s s
get = State (\s -> (s, s))

-- Overwrites the entire state. In other words, wraps a state into a State type emitting nothing.
put :: s -> State s ()
put s = State (\_ -> ((), s))

-- Mutates the entire state and emits nothing.
modify :: (s -> s) -> State s ()
modify f = get >>= \x -> put (f x)

-- Emits, given a state.
evalState :: State s a -> s -> a
evalState act = fst . runState act

-- Retrieves state, given a state.
execState :: State s a -> s -> s
execState act = snd . runState act





