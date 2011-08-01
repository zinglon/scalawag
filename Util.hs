module Util where

import Core

-- lololol
infixl 0 |>
(|>) :: a -> (a -> b) -> b
x |> f = f x

-- lololol x2
infixr 2 ??
(??) :: Maybe a -> a -> a
(??) = flip fromMaybe

infixr  1 ?
(?) :: Bool -> a -> a -> a
(?) b t e = if b then t else e



infixl 4 <?>, =<$, =<*

(<?>) :: (Monad m) => m Bool -> m a -> m a -> m a
(<?>) mb mt me = do b <- mb
                    if b then mt else me

(=<$) :: (Monad m) => (a -> m b) -> (m a -> m b)
f =<$ x = x >>= f

(=<*) :: (Applicative m, Monad m) => m (a -> m b) -> (m a -> m b)
(=<*) f x = join $ f <*> x

noop :: (Monad m) => m ()
noop = return ()

clamp :: (Ord a) => a -> a -> a -> a
clamp lb ub = max lb . min ub


maybeIf :: (a -> Bool) -> a -> Maybe a
maybeIf p x = if p x then Just x else Nothing


-- iterates the given function until the termination check returns true, 
-- starting with the given initial value
iterWhileM :: (Monad m) => (a -> m a) -> m Bool -> a -> m a
iterWhileM lb q x = do x' <- lb x
                       done <- q
                       if done then return x' else iterWhileM lb q x'

repeatWhileM :: (Functor m, Monad m) => (a -> Bool) -> m a -> m [a]
repeatWhileM p mx = step =<< mx
    where step x | p x       = (x:) <$> repeatWhileM p mx
                 | otherwise = return []

