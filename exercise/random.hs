import Prelude hiding (foldr, foldl, getLine, Maybe, Just, Nothing)
import Data.Monoid
import Data.Foldable hiding (sum)
import Data.Char
import Control.Applicative

-- 5.7.5
pyths :: Int -> [(Int, Int, Int)]
pyths n = [(x,y,z) | x <- [1..n], y <- [1..n], z <- [1..n], x*x + y*y == z*z]

-- 5.7.9
scalarproduct xs ys = sum [x*y | (x,y) <- zip xs ys]

-- 6.8.7
merge :: Ord a => [a] -> [a] -> [a] -- param lists sorted
merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) = if x < y then
                         x:(merge xs (y:ys))
                      else
                         y:(merge (x:xs) ys)

-- 6.8.8
msort :: Ord a => [a] -> [a]
msort [] = []
msort [x] = [x]
msort xs = merge l r
           where halves = halve xs
                 l = msort $ fst halves
                 r = msort $ snd halves

halve :: [a] -> ([a],[a])
halve xs = (take half xs, drop half xs)
           where half = (length xs) `div` 2


-- 7.9.2
all' :: (a -> Bool) -> [a] -> Bool
all' p [] = True
all' p (x:xs) = if p x then
                   all' p xs
                else
                   False
any' :: (a -> Bool) -> [a] -> Bool
any' p [] = False
any' p (x:xs) = if p x then
                   True
                else
                   any' p xs

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' p [] = []
takeWhile' p (x:xs) | p x       = x : (takeWhile' p xs)
                    | otherwise = []

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' p [] = []
dropWhile' p (x:xs) | p x       = dropWhile' p xs
                    | otherwise = x:xs

-- 7.9.3
-- foldr :: (a -> b -> b) -> b -> [a] -> b
map' :: (a -> b) -> [a] -> [b]
map' f = foldr ((:) . f) []

filter' :: (a -> Bool) -> [a] -> [a]
filter' p = foldr (\x y -> if p x then x : y else y) []

-- 7.9.4
-- foldl :: (a -> b -> a) -> a -> [b] -> a
-- foldl f v [] = v
-- foldl f v (x:xs) = foldl f (f v x) xs
dec2int :: [Int] -> Int
dec2int = foldl ((+). (*10)) 0


-- 8.9.7
data Maybe a = Nothing | Just a

instance Eq a => Eq (Maybe a) where
    Nothing == Nothing = True
    Nothing == Just _  = False
    Just _ == Nothing = False
    Just x == Just y  = x == y

-- instance Eq a => Eq ([a]) where
--     [] == [] = True 
--     [] == _ = False
--     _ == [] = False
--     (x:xs) == (y:ys) = (x==y) && (xs==ys)


-- 10.9.4
adder :: IO ()
adder = do putStrLn "How many numbers to add together?"
           n <- getInt
           xs <- getInts n
           putStrLn (show $ sum xs)

getInts :: Int -> IO [Int]
getInts 0 = return []
getInts n = do x <- getInt
               xs <- getInts (n-1)
               return (x:xs)


getInt :: IO Int
getInt = do line <- getLine
            if Prelude.all isDigit line then
                return (read line :: Int)
            else
                getInt



getLine :: IO String
getLine = do x <- getChar
             if x == '\n' then
                return []
             else 
                do xs <- getLine
                   return (x:xs)



-- 12.5.7

data Expr a = Var a | Val Int | Add (Expr a) (Expr a)
              deriving Show
-- implement Functor, Applicative and Monad.

-- class Functor f where
--     fmap :: (a -> b) -> f a -> f b
instance Functor Expr where
    fmap g (Var x) = Var (g x)
    fmap g (Val n) = Val n
    fmap g (Add l r) = Add (fmap g l) (fmap g r)

-- class Functor => Applicative f where
--     pure  :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b
instance Applicative Expr where
    pure = Var
    (Var g) <*> fx = fmap g fx
    _ <*> (Val n) = Val n
    -- non-exhaustive

-- class Applicative => Monad m where 
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b
instance Monad Expr where
    return = pure
    Val x   >>= f = Val x
    Var x   >>= f = f x
    Add x y >>= f = Add (x >>= f) (y >>= f)


-- 14.5.4
data DataNodeTree a = Leaf | Node (DataNodeTree a) a (DataNodeTree a)
                      deriving Show


instance Monoid (DataNodeTree a) where
    mempty = Leaf
    Node l v Leaf `mappend` y = Node l v y
    Node l v r `mappend` y = r `mappend` y

-- class Foldable f
--     fold :: Monoid a => f a -> a
--     foldMap :: Monoid b => (a -> b) -> f a -> b
--     foldr :: (a -> b -> b) -> b -> f a -> b
--     foldl :: (a -> b -> a) -> a -> f b -> a

instance Foldable DataNodeTree where
    fold Leaf = mempty
    fold (Node l v r) = (fold l) `mappend` v `mappend` (fold r)

    foldMap f Leaf = mempty
    foldMap f (Node l v r) = (foldMap f l) `mappend` (f v) `mappend` (foldMap f r)

    --foldr g v [] = v
    --foldr g v (x:xs) = g x (foldr g v xs)
    foldr g v Leaf = v
    foldr g v (Node l x r) = foldr g (g x (foldr g v r)) l

    --foldl g v [] = v
    --foldl g v (x:xs) = foldl g (g v x) xs
    foldl g v Leaf = v
    foldl g v (Node l x r) = foldl g (foldl g (g v x) r) l


-- 15.9.2
-- outermost:
-- fst (1+2, 2+3)
-- = 1+2
-- = 3

-- innermost:
-- fst (1+2, 2+3)
-- = fst (3, 2+3)
-- = fst (3, 5)
-- = 3

-- 2 steps vs 3 steps


-- 15.9.3
mult = \x -> (\y -> x * y)

-- mult 3 4
-- = (\x -> (\y -> x * y)) 3 4
-- = (\y -> 3 * y) 4
-- = 3 * 4
-- = 12
