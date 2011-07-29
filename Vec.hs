{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
module Vec where

import Prelude hiding (and)
import Control.Applicative
import Data.Foldable
import Data.List

type Pt2 = Vec Two
type Pt3 = Vec Three

pt2 :: a -> a -> Pt2 a
pt2 x y = x :*: y :*: Nil

pt3 :: a -> a -> a -> Pt3 a
pt3 x y z = x :*: y :*: z :*: Nil

toPair :: Pt2 a -> (a, a)
toPair (x :*: y :*: Nil) = (x, y)


data Z
data S n

type One = S Z
type Two = S One
type Three = S Two
type Four = S Three

infixr 2 :*:
data Vec n a where
    Nil :: Vec Z a
    (:*:) :: a -> Vec n a -> Vec (S n) a


vmap :: (a -> b) -> Vec n a -> Vec n b
vmap _ Nil = Nil
vmap f (x :*: xs) = f x :*: vmap f xs

vzip :: Vec n a -> Vec n b -> Vec n (a, b)
vzip Nil Nil = Nil
vzip (x :*: xs) (y :*: ys) = (x, y) :*: vzip xs ys

vap :: Vec n (a -> b) -> Vec n a -> Vec n b
vap Nil Nil = Nil
vap (f :*: fs) (x :*: xs) = f x :*: vap fs xs

vfoldr :: (a -> b -> b) -> b -> Vec n a -> b
vfoldr _ z Nil = z
vfoldr f z (x :*: xs) = f x (vfoldr f z xs)

class Vector n where
    vec :: a -> Vec n a
    vfromList :: [a] -> Maybe (Vec n a)

instance Vector Z where
    vec _ = Nil
    vfromList [] = Just Nil
    vfromList _ = Nothing

instance (Vector n) => Vector (S n) where
    vec x = x :*: vec x
    vfromList (x:xs) = (x :*:) <$> vfromList xs
    vfromList [] = Nothing


instance Functor (Vec n) where 
    fmap = vmap

instance (Vector n) => Applicative (Vec n) where
    pure = vec
    (<*>) = vap

vzipWith f xs ys = f <$> xs <*> ys

instance Foldable (Vec n) where
    foldr = vfoldr



instance (Show a, Vector n) => Show (Vec n a) where
    show xs = unwords . intersperse ":*:" $ (show <$> toList xs) ++ ["Nil"]

instance (Read a, Vector n) => Read (Vec n a) where
    -- fuck this shit




instance (Eq a, Vector n) => Eq (Vec n a) where
    xs == ys = toList xs == toList ys

instance (Ord a, Vector n) => Ord (Vec n a) where
    compare xs ys = toList xs `compare` toList ys

instance (Num a, Vector n) => Num (Vec n a) where
    (+) = liftA2 (+)
    (-) = liftA2 (-)
    (*) = liftA2 (*)
    negate = fmap negate
    abs    = fmap abs     -- dammit Num
    signum = fmap signum  -- why you do this
    fromInteger = pure . fromInteger -- this is so stupid

instance (Fractional a, Vector n) => Fractional (Vec n a) where
    (/) = liftA2 (/)
    recip = fmap recip
    fromRational = pure . fromRational


