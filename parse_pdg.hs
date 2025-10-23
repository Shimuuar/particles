{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE LexicalNegation       #-}
{-# LANGUAGE MultiWayIf            #-}
{-# LANGUAGE NoFieldSelectors      #-}
{-# LANGUAGE OverloadedRecordDot   #-}
{-# LANGUAGE OverloadedStrings     #-}
module QQ where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.Text    (Text)
import Data.Text    qualified as T
import Data.Text.IO qualified as TIO
import Text.Printf
import Text.ParserCombinators.ReadP qualified as RP


data Measurement = Measurement
  { x  :: !Double
  , ci :: !(Double,Double)
  }
  deriving (Show,Eq)

data Row = Row
  { id    :: !Text
  , mass  :: !(Maybe Measurement)
  , width :: !(Maybe Measurement)
  , name  :: !Text
  , sign  :: !Text
  }
  deriving (Show,Eq)

parseMeasurement :: Text -> Text -> Text -> Maybe Measurement
parseMeasurement s posS negS = case (toD s, toD posS, toD negS) of
  (Just x, Just pos, Just neg) -> Just $ Measurement x (neg, pos)
  (Nothing,Nothing,  Nothing)  -> Nothing
  _ -> error "Invalid field"
  where
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
    

parseRow row =
  [ Row { id    = i
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

parsePDG = do
  txt <- TIO.readFile "mass_width_2025.txt"
  
  pure $ concat [ parseRow row
       | row <- T.lines txt
       , not $ T.all isSpace row
       , T.head row /= '*'
       ]


haskName :: Int -> Text -> Text -> Maybe String
haskName = \cases
  -- Gauge bosons & Higgs
  21  "g"     _ -> Just "Gluon"
  22  "gamma" _ -> Just "Gamma"
  23  "Z"     _ -> Just "Z0"
  24  "W"     _ -> Just "WPlus"
  -24 "W"     _ -> Just "WMinus"
  25  "H"     _ -> Just "Higgs"
  -- Leptons
  11  "e"       _ -> Just "Electron"
  -11 "e"       _ -> Just "Positron"
  13  "mu"      _ -> Just "MuMinus"
  -13 "mu"      _ -> Just "MuPlus"
  15  "tau"     _ -> Just "TauMinus"
  -15 "tau"     _ -> Just "TauPlus"
  12  "nu(e)"   _ -> Just "NuE"
  -12 "nu(e)"   _ -> Just "AntinuE"
  14  "nu(mu)"  _ -> Just "NuMu"
  -14 "nu(mu)"  _ -> Just "AntinuMu"
  16  "nu(tau)" _ -> Just "NuTau"
  -16 "nu(tau)" _ -> Just "AntinuTau"
  -- Quarks
  n q _ | n > 0 && n <  10 -> Just ("Quark"     ++ (toUpper <$> T.unpack q))
        | n < 0 && n > -10 -> Just ("Antiquark" ++ (toUpper <$> T.unpack q))
  -- Hadron zoo
  310  "K(S)" _ -> Just "KShort"
  -310 "K(S)" _ -> Just "AntiKShort"
  130  "K(L)" _ -> Just "KLong"
  -130 "K(L)" _ -> Just "AntiKLong"
  i nm sign -> case RP.readP_to_S (parseName <* RP.eof) (T.unpack nm) of
    (x,""):_ -> Just $ name2hask i sign x
    _        -> Nothing
  _ _ _ -> Nothing


data Name = Name
  { name  :: String
  , s1    :: Maybe String
  , s2    :: Maybe String
  , prime :: Bool
  , star  :: Bool
  }
  deriving (Show)

name2hask :: Int -> Text -> Name -> String
name2hask n sign nm = case nm.name of
  "p" | n > 0     -> "Proton"
      | otherwise -> "Antiproton"
  "n" | n > 0     -> "Neutron"
      | otherwise -> "Antineutron"
  "N" | "+" <- sign -> if | n > 0     -> "N_Plus"  ++ suffix
                          | otherwise -> "N_Minus" ++ suffix
      | "0" <- sign -> if | n > 0     -> "N0"      ++ suffix
                          | otherwise -> "AntiN0"  ++ suffix
  "Lambda" | n > 0     -> "Lambda" ++ suffix
           | otherwise -> "AntiLambda" ++ suffix
  -- Mesons
  "pi" | "+" <- sign -> if | n > 0     -> "PiPlus"  ++ suffix
                           | otherwise -> "PiMinus" ++ suffix
       | "0" <- sign -> "Pi0" ++ suffix
  --     , n > 0       -> "
  --
  _ -> show nm
  where
    suffix = maybe "" ('_':) nm.s1
          ++ maybe "" ('_':) nm.s2


parseName :: RP.ReadP Name
parseName = do
  nm    <- RP.munch1 isAlpha
  s1    <- optional (RP.char '(' *> RP.munch1 isAlphaNum <* RP.char ')')
  prime <- not . null <$> RP.munch (=='\'')
  star  <- not . null <$> RP.munch (=='*')
  s2    <- optional (RP.char '(' *> RP.munch1 isAlphaNum <* RP.char ')')
  return $ Name nm s1 s2 prime star
                  
hasAnti :: Int -> Bool
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



go = do
  xs <- parsePDG
  mapM_ id $ 
    [ printf "%10i | %-20s %4s | %s\n"
      i
      r.name
      r.sign
      (maybe "" id $ haskName i r.name r.sign)
    | r <- xs
    , i <- let i0 = read $ T.unpack r.id in if hasAnti i0 then [i0,-i0] else [i0]
    ]

  
