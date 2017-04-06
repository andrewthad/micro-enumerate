{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE FlexibleContexts #-}

module Data.Enumerable 
  ( Enumerable(..)
  , boundedEnumerated
  ) where

import GHC.Generics
import Data.Int
import Data.Word
import Data.Void
import Data.Functor.Identity
import Control.Applicative

class Enumerable a where
  enumerated :: [a]
  default enumerated :: (Generic a, GEnumerable (Rep a)) => [a]
  enumerated = to <$> genumerated

boundedEnumerated :: (Bounded a, Enum a) => [a]
boundedEnumerated = [minBound..maxBound]

instance Enumerable Void
instance Enumerable ()
instance Enumerable Bool
instance Enumerable Ordering
instance Enumerable Int8 where
  enumerated = boundedEnumerated
instance Enumerable Int16 where
  enumerated = boundedEnumerated
instance Enumerable Int32 where
  enumerated = boundedEnumerated
instance Enumerable Int64 where
  enumerated = boundedEnumerated
instance Enumerable Int where
  enumerated = boundedEnumerated
instance Enumerable Word8 where
  enumerated = boundedEnumerated
instance Enumerable Word16 where
  enumerated = boundedEnumerated
instance Enumerable Word32 where
  enumerated = boundedEnumerated
instance Enumerable Word64 where
  enumerated = boundedEnumerated
instance Enumerable Word where
  enumerated = boundedEnumerated
instance (Enumerable a) => Enumerable (Identity a) where
  enumerated = Identity <$> enumerated
instance (Enumerable a) => Enumerable (Const a b) where
  enumerated = Const <$> enumerated
instance Enumerable Char where
  enumerated = boundedEnumerated
instance (Enumerable a, Enumerable b) => Enumerable (Either a b) where
  enumerated = (Left <$> enumerated) ++ (Right <$> enumerated)
instance (Enumerable a) => Enumerable (Maybe a) where
  enumerated = Nothing : (Just <$> enumerated)
instance (Enumerable a, Enumerable b) => Enumerable (a, b) where
  enumerated = (,) <$> enumerated <*> enumerated

class GEnumerable f where
  genumerated :: [f x]

instance GEnumerable V1 where
  genumerated = []

instance GEnumerable U1 where
  genumerated = [U1]

instance (Enumerable a) => GEnumerable (K1 x a) where
  genumerated = K1 <$> enumerated

instance (GEnumerable f, GEnumerable g) => GEnumerable (f :+: g) where
  genumerated = map L1 genumerated ++ map R1 genumerated

instance (GEnumerable f) => GEnumerable (M1 x t f) where
 genumerated = M1 <$> genumerated

