module Data.AccNotZeroOr(
  AccNotZeroOr(..)
, _IsAccNotZero
, _OrAccNotZero
, isoAccNotZeroOr
, OneNotZeroOr
, isoOneNotZeroOr
, isoOneNotZeroOrNumber
, isoOneNotZeroOrT
) where

import Control.Applicative(Applicative(pure, (<*>)))
import Control.Category(Category((.)))
import Control.Lens(Prism, prism, Iso, iso)
import Data.Either(Either(Left, Right))
import Data.Eq(Eq)
import Data.Functor.Alt
import Data.Functor.Identity(Identity(Identity))
import Data.Monoid(Monoid(mappend, mempty))
import Data.NotZero(NotZero)
import Data.NotZeroOr(NotZeroOr(IsNotZero, OrNotZero), NotZeroOrT, isoNumber, isoNotZeroOrT)
import Data.Semigroup(Semigroup((<>)))
import Prelude(Num)

data AccNotZeroOr f a x =
  IsAccNotZero (f (NotZero a))
  | OrAccNotZero x

_IsAccNotZero ::
  Prism (AccNotZeroOr f a x) (AccNotZeroOr f b x) (f (NotZero a)) (f (NotZero b))
_IsAccNotZero =
  prism
    IsAccNotZero
    (\z -> case z of
              IsAccNotZero o ->
                Right o
              OrAccNotZero x ->
                Left (OrAccNotZero x))

_OrAccNotZero ::
  Prism (AccNotZeroOr f a x) (AccNotZeroOr f a y) x y
_OrAccNotZero =
  prism
    OrAccNotZero
    (\z -> case z of
              IsAccNotZero o ->
                Left (IsAccNotZero o)
              OrAccNotZero x ->
                Right x)

isoAccNotZeroOr ::
  Iso (AccNotZeroOr f a x) (AccNotZeroOr g a x) (Either (f (NotZero a)) x) (Either (g (NotZero a)) x)
isoAccNotZeroOr =
  iso
    (\z -> case z of
              IsAccNotZero o ->
                Left o
              OrAccNotZero x ->
                Right x)
    (\e -> case e of
              Left o ->
                IsAccNotZero o
              Right x ->
                OrAccNotZero x)

type OneNotZeroOr a x =
  AccNotZeroOr Identity a x

isoOneNotZeroOr ::
  Iso (OneNotZeroOr a x) (OneNotZeroOr b y) (NotZeroOr a x) (NotZeroOr b y) 
isoOneNotZeroOr =
  iso
    (\z -> case z of
              IsAccNotZero (Identity o) ->
                IsNotZero o
              OrAccNotZero x ->
                OrNotZero x)
    (\z -> case z of
              IsNotZero o ->
                IsAccNotZero (Identity o)
              OrNotZero x ->
                OrAccNotZero x)

isoOneNotZeroOrNumber ::
  (Eq a, Num a) =>
  Iso (OneNotZeroOr a ()) (OneNotZeroOr a ()) a a
isoOneNotZeroOrNumber =
  isoOneNotZeroOr . isoNumber

isoOneNotZeroOrT ::
  Iso (OneNotZeroOr a x) (OneNotZeroOr b y) (NotZeroOrT a Identity x) (NotZeroOrT b Identity y)
isoOneNotZeroOrT =
  isoOneNotZeroOr . isoNotZeroOrT

instance Semigroup (AccNotZeroOr f a x) where
  OrAccNotZero x <> _ =
    OrAccNotZero x
  IsAccNotZero _ <> y =
    y
    
instance Monoid x => Monoid (AccNotZeroOr f a x) where
  mappend =
    (<>)
  mempty =
    OrAccNotZero mempty

instance Functor f => Functor (AccNotZeroOr f a) where
  fmap _ (IsAccNotZero z) =
    IsAccNotZero z
  fmap f (OrAccNotZero x) =
    OrAccNotZero (f x)

instance Alt f => Apply (AccNotZeroOr f a) where
  IsAccNotZero z1 <.> IsAccNotZero z2 =
    IsAccNotZero (z1 <!> z2)
  IsAccNotZero z1 <.> OrAccNotZero _ =
    IsAccNotZero z1
  OrAccNotZero _ <.> IsAccNotZero z2 =
    IsAccNotZero z2
  OrAccNotZero f <.> OrAccNotZero a =
    OrAccNotZero (f a)
                  
instance Alt f => Applicative (AccNotZeroOr f a) where
  pure =
    OrAccNotZero
  (<*>) =
    (<.>)
