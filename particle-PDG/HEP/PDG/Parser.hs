{-# LANGUAGE OverloadedStrings #-}
-- |
-- Parsers for data provided by PDG
module HEP.PDG.Parser
  ( -- * Types
    Measurement(..)
    -- * Mass-width data
  , RowMW(..)
  , parseMassWidth
  ) where

import Data.Char
import Data.Text    (Text)
import Data.Text    qualified as T
import Data.Text.IO qualified as TIO



----------------------------------------------------------------
-- Types
----------------------------------------------------------------

-- | Measurements result for some value
data Measurement = Measurement
  { x  :: !Double          -- ^ Central value
  , ci :: !(Double,Double) -- ^ Confidence interval 
  }
  deriving (Show,Eq)

----------------------------------------------------------------
-- Mass-width-MC ID data
----------------------------------------------------------------

-- | Row of mass-width data
data RowMW = RowMW
  { id    :: !Text                -- ^ MC id of a particle
  , mass  :: !(Maybe Measurement)
  , width :: !(Maybe Measurement)
  , name  :: !Text
  , sign  :: !Text                -- ^ Electric charge
  }
  deriving (Show,Eq)



-- Parse triple of values as Measurement. Same order as they appear in
-- data file
parseMeasurement
  :: Text -- Central value
  -> Text -- Upper bound
  -> Text -- Lower bount
  -> Maybe Measurement
parseMeasurement center posS negS = case (toD center, toD posS, toD negS) of
  (Just x, Just pos, Just neg) -> Just $ Measurement x (neg, pos)
  (Nothing,Nothing,  Nothing)  -> Nothing
  _ -> error "Invalid field"
  where
    -- Numbers use a bit idiosyncratic notation. We can't simply use
    -- read.
    toD :: T.Text -> Maybe Double
    toD s = case T.unpack $ T.strip s of
      ""     -> Nothing
      '+':ss -> Just $ str2D $ norm ss
      ss     -> Just $ str2D $ norm ss
    norm = \case
      '.':'E':xs -> '.':'0':'E':xs
      x:xs       -> x : norm xs
      []         -> []
    str2D s = case reads s of
      [(x,"")] -> x
      _        -> error $ "Cannot parse '" ++ s ++ "'"


-- Parse single row of data. This is column based format
--
-- Each row may contain up to 4 particles with different signs and MC IDs
parseRow :: Text -> [RowMW]
parseRow row =
  [ RowMW { id    = i
          , mass  = mass
          , width = width
          , name  = name
          , sign  = s
          }
  | (i,s) <- id_signs
  ]
  where
    rng i j = T.take (j-i+1) $ T.drop (i-1) row
    id_list = ( T.strip $ rng 1 8
              , T.strip $ rng 9 16
              , T.strip $ rng 17 24
              , T.strip $ rng 25 32
              )
    mass  = parseMeasurement (rng 34 51) (rng 53 60) (rng 62 69)
    width = parseMeasurement (rng 71 88) (rng 90 97) (rng 99 106)
    (name,signs) = case T.words $ rng 108 128 of
      [n,s] -> (n,s)
      xs    -> error $ "Cannot parse name field of PDG data" ++ show xs
    id_signs = case (id_list, T.splitOn "," signs) of
      ((i1,"","",""), [s1])          -> [(i1,s1)]
      ((i1,i2,"",""), [s1,s2])       -> [(i1,s1), (i2,s2)]
      ((i1,i2,i3,""), [s1,s2,s3])    -> [(i1,s1), (i2,s2), (i3,s3)]
      ((i1,i2,i3,i4), [s1,s2,s3,s4]) -> [(i1,s1), (i2,s2), (i3,s3), (i4,s4)]
      _ -> error "Cannot parse signs"


-- | Parse data file with mass width data. All parse failures are
--   reported as exceptions.
--
--   It's able to parse 2025 data but parser is quite
--   fragile. Failures with future data are possible.
parseMassWidth :: FilePath -> IO [RowMW]
parseMassWidth path = do
  txt <- TIO.readFile path  
  pure $ concat [ parseRow row
       | row <- T.lines txt
       , not $ T.all isSpace row
       , T.head row /= '*'
       ]
