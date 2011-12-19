{-# LANGUAGE TypeFamilies
           , MultiParamTypeClasses
           , FlexibleInstances
           , DeriveFunctor
           , DeriveDataTypeable
  #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Data.AffineSpace.Point
-- Copyright   :  (c) 2011 Brent Yorgey
-- License     :  BSD-style (see LICENSE)
-- Maintainer  :  byorgey@cis.upenn.edu
--
-- A type for /points/ (as distinct from vectors), with an appropriate
-- AffineSpace instance.
--
-----------------------------------------------------------------------------

module Data.AffineSpace.Point
       ( -- * Points

         Point(..), origin, (*.)

       ) where

import Data.VectorSpace
import Data.AffineSpace

import Control.Newtype
import Data.Data (Data)
import Data.Typeable (Typeable)

------------------------------------------------------------
--  Points  ------------------------------------------------
------------------------------------------------------------

-- | @Point@ is a newtype wrapper around vectors that we wish to treat
--   as points, so we don't get them mixed up. The distinction is
--   important: translations affect points, but leave vectors
--   unchanged.  Points are instances of the 'AffineSpace' class from
--   "Data.AffineSpace".
newtype Point v = P v
  deriving (Eq, Ord, Read, Show, Data, Typeable, Functor)

instance Newtype (Point v) v where
  pack = P
  unpack (P v) = v

-- | The origin of the vector space @v@.
origin :: AdditiveGroup v => Point v
origin = P zeroV

instance AdditiveGroup v => AffineSpace (Point v) where
  type Diff (Point v) = v
  P v1 .-. P v2 = v1 ^-^ v2
  P v1 .+^ v2   = P (v1 ^+^ v2)

-- | Scale a point by a scalar.
(*.) :: VectorSpace v => Scalar v -> Point v -> Point v
s *. P v = P (s *^ v)
