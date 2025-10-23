{-# LANGUAGE MagicHash #-}
-- |
-- Type classes for defining particle parameters.
module HEP.PDG.Class where

import GHC.Exts (Proxy#, proxy#)


class Mass a where
  basicMass :: Proxy# a -> Double
  basicMassCI :: Proxy# a -> (Double, Double)


class Width a where
  basicWidth :: Proxy# a -> Double
  basicWidthCI :: Proxy# a -> (Double, Double)
