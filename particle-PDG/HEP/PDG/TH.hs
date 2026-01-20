-- |
-- TH generate for data types representing particles. A lot of code is
-- devoted to converting particle name into identifier which could be
-- type or constructor name
module HEP.PDG.TH
  ( -- * Particle properties
    hasAnti
    -- * Haskell name generation
  , Name(..)
  , parseName
  ) where

import Control.Applicative
import Data.Char
import Text.Printf
import Text.ParserCombinators.ReadP qualified as RP

----------------------------------------------------------------
-- Particle properties
----------------------------------------------------------------

-- | Check whether particle has distinct antiparticle.
hasAnti
  :: Int -- ^ MC ID
  -> Bool
hasAnti n
  | n <= 0    = error "hasAnti: negative ID"
  -- Quarks & leptons
  | n < 20    = True
  -- Gauge bosons
  | n == 24   = True
  | n < 30    = False
  -- Hadrons zoo
  | quarkonia = False
  | otherwise = True
  where
    quarkonia = ((n `rem` 10000) `quot` 10) `elem` [11,22,33,44]


----------------------------------------------------------------
-- Name generation
----------------------------------------------------------------

-- | Parsed representation of particle name
data Name = Name
  { name  :: String       -- ^ Base name of particle
  , s1    :: Maybe String -- ^ Subscript 1
  , s2    :: Maybe String -- ^ Subscript 2 (or value in parens)
  , prime :: Bool         -- ^ Whether particle is primed
  , star  :: Bool         -- ^ Whether particle has @*@ superscript
  }
  deriving (Show)

-- | Parser for particle name as it appears in mass-width tables
parseName :: RP.ReadP Name
parseName = do
  nm    <- RP.munch1 (\c -> isAlpha c || c =='/')
  s1    <- optional (RP.char '(' *> RP.munch1 isAlphaNum <* RP.char ')')
  prime <- not . null <$> RP.munch (=='\'')
  star  <- not . null <$> RP.munch (=='*')
  s2    <- optional (RP.char '(' *> RP.munch1 isAlphaNum <* RP.char ')')
  return $ Name nm s1 s2 prime star

-- makeHaskellName :: Row с
