module Data.NotZeroOr(
  NotZeroOr(..)
, _IsNotZero
, _OrNotZero
, isoNotZeroOr
, Number
, getNumber
, isoNumber
, NotZeroOrT(..)
, isoNotZeroOrT
) where

import Control.Applicative(Applicative(pure, (<*>)), liftA2)
import Control.Category(Category((.)))
import Control.Lens(Prism, prism, Iso, iso, (^?))
import Control.Monad(Monad(return, (>>=)))
import Control.Monad.Trans.Class(MonadTrans(lift))
import Data.Either(Either(Left, Right))
import Data.Eq(Eq)
import Data.Functor(Functor(fmap))
import Data.Functor.Apply(Apply((<.>)), liftF2)
import Data.Functor.Bind(Bind((>>-)))
import Data.Functor.Bind.Trans(BindTrans(liftB))
import Data.Functor.Identity(Identity(Identity))
import Data.Maybe(Maybe(Nothing, Just))
import Data.NotZero(NotZero, notZero, getNotZero)
import Data.Ord(Ord)
import Prelude(Show, Num)

data NotZeroOr a x =
  IsNotZero (NotZero a)
  | OrNotZero x
  deriving (Eq, Ord, Show)

_IsNotZero ::
  Prism (NotZeroOr a x) (NotZeroOr b x) (NotZero a) (NotZero b)
_IsNotZero =
  prism
    IsNotZero
    (\z -> case z of
              IsNotZero o ->
                Right o
              OrNotZero x ->
                Left (OrNotZero x))

_OrNotZero ::
  Prism (NotZeroOr a x) (NotZeroOr a y) x y
_OrNotZero =
  prism
    OrNotZero
    (\z -> case z of
              IsNotZero o ->
                Left (IsNotZero o)
              OrNotZero x ->
                Right x)

isoNotZeroOr ::
  Iso (NotZeroOr a x) (NotZeroOr a x) (Either (NotZero a) x) (Either (NotZero a) x)
isoNotZeroOr =
  iso
    (\z -> case z of
              IsNotZero o ->
                Left o
              OrNotZero x ->
                Right x)
    (\e -> case e of
              Left o ->
                IsNotZero o
              Right x ->
                OrNotZero x)

type Number a =
  NotZeroOr a ()

getNumber ::
  Num a =>
  Number a
  -> a
getNumber (IsNotZero o) =
  getNotZero o
getNumber (OrNotZero ()) =
  0

isoNumber ::
  (Eq a, Num a) =>
  Iso (Number a) (Number a) a a
isoNumber =
  iso
    getNumber
    (\a -> case a ^? notZero of
             Nothing -> OrNotZero ()
             Just z -> IsNotZero z)

instance Functor (NotZeroOr a) where
  fmap _ (IsNotZero z) =
    IsNotZero z
  fmap f (OrNotZero x) =
    OrNotZero (f x)

instance Apply (NotZeroOr a) where
  IsNotZero z <.> _ =
    IsNotZero z
  OrNotZero _ <.> IsNotZero z =
    IsNotZero z
  OrNotZero f <.> OrNotZero a =
    OrNotZero (f a)

instance Applicative (NotZeroOr a) where
  pure =
    OrNotZero
  (<*>) =
    (<.>)

instance Bind (NotZeroOr a) where
  IsNotZero z >>- _ =
    IsNotZero z
  OrNotZero x >>- f =
    f x

instance Monad (NotZeroOr a) where
  return =
    pure
  (>>=) =
    (>>-)

newtype NotZeroOrT a f x =
  NotZeroOrT (f (NotZeroOr a x))

isoNotZeroOrT ::
  Iso (NotZeroOr a x) (NotZeroOr b y) (NotZeroOrT a Identity x) (NotZeroOrT b Identity y)
isoNotZeroOrT =
  iso
    (NotZeroOrT . Identity)
    (\(NotZeroOrT (Identity x)) -> x)

instance Functor f => Functor (NotZeroOrT a f) where
  fmap f (NotZeroOrT q) =
    NotZeroOrT (fmap (fmap f) q)

instance Apply f => Apply (NotZeroOrT a f) where
  NotZeroOrT f <.> NotZeroOrT a =
    NotZeroOrT (liftF2 (<*>) f a)

instance Applicative f => Applicative (NotZeroOrT a f) where
  pure =
    NotZeroOrT . pure . pure
  NotZeroOrT f <*> NotZeroOrT a =
    NotZeroOrT (liftA2 (<*>) f a)

bind ::
  Monad f =>
  NotZeroOrT a f x ->
  (x -> NotZeroOrT a f y)
  -> NotZeroOrT a f y
NotZeroOrT q `bind` f =
    NotZeroOrT (q >>= \n -> case n of
                              IsNotZero z ->
                                return (IsNotZero z)
                              OrNotZero x ->
                                let NotZeroOrT r = f x
                                in r)

instance (Apply f, Monad f) => Bind (NotZeroOrT a f) where
  (>>-) =
    bind

instance Monad f => Monad (NotZeroOrT a f) where
  return =
    NotZeroOrT . return . return
  (>>=) =
    bind

instance BindTrans (NotZeroOrT a) where
  liftB =
    NotZeroOrT . fmap OrNotZero

instance MonadTrans (NotZeroOrT a) where
  lift =
    NotZeroOrT . fmap OrNotZero
