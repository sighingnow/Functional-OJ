module Stream where

import Control.Arrow
import Control.Applicative

-- import Stream.Internal

data Stream a = a :> Stream a
infixr :>

-- Defined in Stream.Internal:
--     data Stream a = a :> Stream a
--     infixr :>

-- | Get the first element of a stream.
headS :: Stream a -> a
headS (a :> _) = a

-- | Drop the first element of a stream.
tailS :: Stream a -> Stream a
tailS (_ :> s) = s

-- {{{ Stream constructors

-- | Construct a stream by repeating a value.
repeatS :: a -> Stream a
repeatS a = a :> repeatS a

-- | Construct a stream by repeatedly applying a function.
iterateS :: (a -> a) -> a -> Stream a
iterateS f x = x :> iterateS f (f x)

-- | Construct a stream by repeating a list forever.
cycleS :: [a] -> Stream a
cycleS [] = error "Cannot construct stream by empty cycle."
cycleS (x:xs) = x :> cycleS (xs ++ [x]) -- TODO optimize

-- | Construct a stream by counting numbers starting from a given one.
fromS :: Num a => a -> Stream a
fromS x = x :> fromS (x + 1)

-- | Same as 'fromS', but count with a given step width.
fromStepS :: Num a => a -> a -> Stream a
fromStepS x s = x :> fromStepS (x + s) s

-- }}}

-- | Fold a stream from the left.
foldrS :: (a -> b -> b) -> Stream a -> b
foldrS f (x :> xs) = f x $ foldrS f xs

-- | Filter a stream with a predicate.
filterS :: (a -> Bool) -> Stream a -> Stream a
filterS p (x :> xs) = if p x then x :> filterS p xs
                             else filterS p xs

-- | Take a given amount of elements from a stream.
takeS :: Int -> Stream a -> [a]
takeS n (x :> xs)
  | n <= 0 = []
  | otherwise = x : takeS (n - 1) xs

-- | Drop a given amount of elements from a stream.
dropS :: Int -> Stream a -> Stream a
dropS n (x :> xs)
  | n <= 0 = x :> xs
  | otherwise = dropS (n - 1) xs

-- | Do take and drop simultaneous.
splitAtS :: Int -> Stream a -> ([a], Stream a)
splitAtS n (x :> xs)
  | n <= 0 = ([], x :> xs)
  | otherwise = let (x', xs') = splitAtS (n-1) xs in (x : x', xs')

-- | Combine two streams with a function.
zipWithS :: (a -> b -> c) -> Stream a -> Stream b -> Stream c
zipWithS f (x :> xs) (y :> ys) = f x y :> zipWithS f xs ys

zipS :: Stream a -> Stream b -> Stream (a, b)
zipS = zipWithS (,)

instance Functor Stream where
    -- fmap :: (a -> b) -> Stream a -> Stream b
    fmap f (x :> xs) = f x :> fmap f xs

instance Applicative Stream where
    -- pure :: a -> Stream a
    pure a = a :> pure a

    -- (<*>) :: Stream (a -> b) -> Stream a -> Stream b
    (<*>) (f :> _) xs = fmap f xs

-- | The stream of fibonacci numbers.
fibS :: Stream Integer
fibS = 0 :> 1 :> zipWithS (+) fibS (tailS fibS)

-- | The stream of prime numbers.
primeS :: Stream Integer
primeS = sieve $ fromS 2
  where sieve (x :> xs) = x :> (sieve $ filterS (\t -> t `mod` x /= 0) xs)
