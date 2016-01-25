{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Data.Functor.Validation where

import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Functor.Contravariant.Generic
import Data.Monoid
import Data.Proxy
import Data.Void (absurd)
import qualified Control.Category as C
import Data.Typeable
import GHC.Generics

newtype Validation e s = Validation { getValidation :: s -> e }

instance C.Category Validation where
    id = Validation id
    (Validation f) . (Validation g) = Validation $ g . f

instance Monoid e => Monoid (Validation e s) where
    mempty = Validation $ const mempty
    mappend (Validation f) (Validation g) = Validation $ \x -> f x <> g x

instance Contravariant (Validation e) where
  contramap f (Validation g) = Validation (g . f)

instance Monoid e => Divisible (Validation e) where
  divide split (Validation f) (Validation g)
    = Validation (\a -> let (u, v) = split a in f u <> g v)
  conquer = Validation (\_ -> mempty)

instance Monoid e => Decidable (Validation e) where
  choose k (Validation f) (Validation g)
    = Validation ( either f g . k)
  lose k = Validation (absurd . k)

class Validate e s where
  validate :: s -> e

{-instance Validate [String] (String, In)-}

gvalidate :: forall e s. (Monoid e, Deciding (Validate e) s) => s -> e
gvalidate = getValidation $ deciding (Proxy :: Proxy (Validate e)) (Validation validate)

data Type = Type TypeRep
data MaxLength = MaxLength Int
data Max a = Max a

instance Validate (Validation All [a] ) MaxLength where
  validate (MaxLength x) = Validation $ \xs -> All (length xs < x)

instance Validate (Validation All [Int]) (Max Int) where
  validate (Max x) = Validation $ \ys -> All $  all (< x) ys

instance Typeable a => Validate (Validation All a) Type where
  validate (Type t) = Validation $ \x -> All (typeOf x == t)

data Schema = Schema
  { type_     :: Type
  , maxLength :: MaxLength
  , maximum_  :: Max Int
  } deriving (Generic)

eg :: Schema
eg = Schema (Type $ typeOf ls) (MaxLength 10) (Max 20)

ls :: [Int]
ls = [1..30]

t1 :: Validation All [Int]
t1 = gvalidate eg

t2 = getValidation t1 ls
