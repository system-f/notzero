module Data.NotZero(
  NotZero
, getNotZero
, notZero
, notZeroElse
, notZero1
) where

import Control.Lens(Prism', prism')
import Data.Monoid(Monoid(mappend, mempty))
import Data.Semigroup(Semigroup((<>)))
import Data.Bool(bool)
import Data.Eq(Eq((==)))
import Data.Maybe(Maybe(Just, Nothing))
import Data.Ord(Ord)
import Prelude(Num((*)), Show)

newtype NotZero a =
  NotZero a
  deriving (Eq, Ord, Show)

getNotZero ::
  NotZero a
  -> a
getNotZero (NotZero a) =
  a

notZero :: 
  (Eq a, Num a) =>
  Prism' a (NotZero a)
notZero =
  prism'
    getNotZero
    (\a -> bool (Just (NotZero a)) Nothing (a == 0))

notZeroElse ::
  (Eq a, Num a) =>
  NotZero a
  -> a
  -> NotZero a
notZeroElse d a =
  bool (NotZero a) d (a == 0)
  
notZero1 ::
  (Eq a, Num a) =>
  NotZero a
notZero1 =
  NotZero 1

instance Num a => Semigroup (NotZero a) where
  NotZero a <> NotZero b = 
    NotZero (a * b)

instance Num a => Monoid (NotZero a) where
  mappend =
    (<>)
  mempty =
    NotZero 1
