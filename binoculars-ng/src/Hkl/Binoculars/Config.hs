{-# LANGUAGE CPP                        #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}

{-
    Copyright  : Copyright (C) 2014-2026 Synchrotron SOLEIL
                                         L'Orme des Merisiers Saint-Aubin
                                         BP 48 91192 GIF-sur-YVETTE CEDEX
    License    : GPL3+

    Maintainer : Picca Frédéric-Emmanuel <picca@synchrotron-soleil.fr>
    Stability  : Experimental
    Portability: GHC only (not tested)
-}

module Hkl.Binoculars.Config
    ( Angstrom(..)
    , Attenuation(..)
    , BinocularsPreConfig(..)
    , Capabilities(..)
    , ConfigRange(..)
    , Degree(..)
    , DestinationTmpl(..)
    , DynamicMask(..)
    , FieldEmitter(..)
    , FieldParsable(..)
    , FlatLocation(..)
    , HasFieldComment(..)
    , HasFieldValue(..)
    , HasIniConfig(..)
    , HasIniParser(..)
    , InputRange(..)
    , InputTmpl(..)
    , InputType(..)
    , InputTypeDeprecated(..)
    , Limits(..)
    , MaskLocation(..)
    , Meter(..)
    , NCores(..)
    , PolarisationCorrection(..)
    , ProjectionType(..)
    , Resolutions(..)
    , RLimits(..)
    , SampleAxis(..)
    , auto
    , auto'
    , autoJSON
    , destination'
    , elemFDef
    , elemFDef'
    , elemFMbDef
    , elemFMbDef'
    , eitherF
    , files
    , getCapabilities
    , getFlat
    , getInitialScannumber
    , getMask
    , getPreConfig
    , iniParser'GeometryValues
    , iniParser'GeometryShifts
    , iniParser'Maybe'Geometry
    , mergeIni
    , parse'
    , parseFDef
    , parseMb
    , parseMbDef
    , readConfig
    ) where


import           Control.Applicative               (many, optional, (<|>))
import           Control.Monad.Catch               (MonadCatch, MonadThrow,
                                                    throwM, try)
import           Control.Monad.Catch.Pure          (runCatch)
import           Control.Monad.IO.Class            (MonadIO)
import           Data.Aeson                        (FromJSON (..), ToJSON (..),
                                                    eitherDecodeStrict, encode)
import           Data.Attoparsec.Text              (Parser, char, decimal,
                                                    double, parseOnly, peekChar,
                                                    satisfy, sepBy, signed,
                                                    skipSpace, takeText,
                                                    takeTill, (<?>))
import           Data.ByteString.Lazy              (toStrict)
import           Data.Char                         (isSpace)
import           Data.Either.Combinators           (maybeToRight)
import           Data.Either.Extra                 (mapLeft, mapRight)
#if !MIN_VERSION_base(4, 20, 0)
import           Data.Foldable                     (foldl')
#endif
import           Data.Hashable                     (Hashable)
import           Data.HashMap.Strict               (HashMap, unionWith)
import           Data.Ini                          (Ini (..), printIni)
import           Data.Ini.Config                   (IniParser, fieldMbOf,
                                                    fieldOf, parseIniFile,
                                                    section, sectionDef,
                                                    sectionMb)
import           Data.Ini.Config.Bidir             (FieldValue (..), bool,
                                                    listWithSeparator, number,
                                                    text)
import           Data.List                         (elemIndex, find, isInfixOf)
import           Data.List.NonEmpty                (NonEmpty (..), head, map)
import           Data.Map.Strict                   (Map, empty)
import           Data.Maybe                        (catMaybes, fromMaybe)
import           Data.Text                         (Text, breakOn, cons, drop,
                                                    empty, findIndex,
                                                    intercalate, isInfixOf,
                                                    length, lines, pack,
                                                    replace, singleton, strip,
                                                    take, takeWhile, toLower,
                                                    unlines, unpack, unwords,
                                                    words)
import           Data.Text.Encoding                (decodeUtf8, encodeUtf8)
import           Data.Text.IO                      (readFile)
import           Data.Typeable                     (Proxy (..), Typeable,
                                                    typeRep)
import           GHC.Conc                          (getNumCapabilities,
                                                    getNumProcessors)
import           GHC.Exts                          (IsList (..))
import           GHC.Generics                      (Generic)
import           Numeric.Interval                  (Interval, empty, hull, inf,
                                                    singleton, singular, sup,
                                                    (...))
import           Numeric.Units.Dimensional.NonSI   (angstrom)
import           Numeric.Units.Dimensional.Prelude (Length, degree, meter, (*~),
                                                    (/~))
import           Path                              (Abs, Dir, File, Path,
                                                    fileExtension, fromAbsDir,
                                                    parseAbsDir, toFilePath)
import           Path.IO                           (getCurrentDir, walkDirAccum)
import           System.Directory                  (doesPathExist)
import           System.FilePath                   (splitExtensions)
import           Test.QuickCheck                   (Arbitrary (..), elements,
                                                    oneof)
import           Text.Printf                       (printf)

import           Prelude                           hiding (drop, length, lines,
                                                    putStr, readFile, take,
                                                    takeWhile, unlines, unwords)

import           Hkl.C.Binoculars
import           Hkl.Detector
import           Hkl.Exception
import           Hkl.Geometry
import           Hkl.Lattice
import           Hkl.Repa
import           Hkl.Types
import           Paths_hkl

-- TODO
-- rendre le parametre surface_orientation obligatoire pour les q
-- revoir le message d'erreur lorsque un des moteur est manquant, c'est incomprehensible... exemple:
--   "CanNotOpenDataSource'Double'Or (CanNotOpenDataSource'Double'Ini \"geometry.values\" \"beta\") (CanNotOpenDataSource'Double'Or (CanNotOpenDataSource'Double'Ini \"geometry.values\" \"mu\") (CanNotOpenDataSource'Double'Or (CanNotOpenDataSource'Double'Ini \"geometry.values\" \"eix\") (HklDataSourceException'HklH5Exception (CanNotOpenH5Or (CanNotOpenDataset \"scan_data/eix\") (CanNotOpenDataset \"SIXS/i14-c-cx1-dt-det_tx.1/position_pre\")))))"
--   ici le probleme c'est eix...
--	$(PROCESS) /nfs/ruche-sixs/sixs-soleil/com-sixs/2023/Run2/Mozhzhukhina_20220548/binoculars/config_sbs_eiger.txt 640
-- gerer les mask en int64 uint64 etc...
--   /nfs/ruche-sixs/sixs-soleil/com-sixs/2020/Run3/Lacaze/mask_eiger.npy
-- comment documenter la section geometry.values
--   migrer le code de config-ini vers ini afin de resoudre ce probleme.
-- gerer les data sources qui n'ont pas la meme dimensionnalite genre un scalar et un array.
-- implementer les q/tth_scantime et q/tth_scanindex
-- implementer les corrections de polarisation
-- gui merge de cube.
-- ajouter un XpadFlat -> ajouter 3.2 dans le nom
-- adapter angles -> cglm 0 -> subprojection
--  - delta_lab, gamma_lab, <sample-axis>
--  - gamma_lab, delta_lab, <sample_axis>


word :: Parser Text
word = takeTill isSpace <?> "word"

-- Class FieldEmitter

class FieldEmitter a where
  fieldEmitter :: a -> Text

-- Class FieldParsable

class FieldEmitter a => FieldParsable a where
  fieldParser :: Parser a

-- class HasFieldComment

class HasFieldValue a => HasFieldComment a where
  fieldComment :: a -> [Text]

-- Class HasFieldValue

class HasFieldValue a where
  fieldvalue :: FieldValue a

auto :: HasFieldValue a => FieldValue a
auto = fieldvalue

auto' :: HasFieldValue a => Text -> Either String a
auto' = fvParse fieldvalue

autoJSON :: (FromJSON a, ToJSON a) => FieldValue a
autoJSON =
    FieldValue { fvParse = eitherDecodeStrict . encodeUtf8
               , fvEmit = decodeUtf8 . toStrict . encode
               }


instance HasFieldValue Bool where
  fieldvalue = bool

instance HasFieldValue Degree where
  fieldvalue = FieldValue
    { fvParse =  mapRight (Degree . (*~ degree)) . fvParse auto
    , fvEmit = \(Degree m) -> pack . show . (/~ degree) $ m
    }

number' :: (Show a, Read a, Num a, Typeable a) => FieldValue a
number' = Data.Ini.Config.Bidir.number
  { fvParse = \t -> case fvParse Data.Ini.Config.Bidir.number . uncomment $ t of
                     Left _ -> fvParse Data.Ini.Config.Bidir.number . uncomment $ nt
                       where
                         nt :: Text
                         nt = cons '0' t
                     Right v -> Right v
  }

instance HasFieldValue Double where
  fieldvalue = number'

instance  HasFieldValue (Detector Hkl DIM2) where
  fieldvalue = FieldValue
               { fvParse = parseDetector2D . strip . uncomment
               , fvEmit = \(Detector2D _ name _) -> pack name
               }

instance HasFieldValue Float where
  fieldvalue = number'

instance HasFieldValue Int where
  fieldvalue = number'

instance HasFieldValue (Path Abs Dir) where
  fieldvalue = FieldValue { fvParse = \t -> mapLeft show (runCatch . parseAbsDir . unpack $ t)
                          , fvEmit = pack . fromAbsDir
                          }

instance HasFieldValue Text where
  fieldvalue = text

instance HasFieldValue [Double] where
  fieldvalue = listWithSeparator "," auto

pairWithSeparator' :: FieldValue l -> Text -> FieldValue r -> FieldValue (l, r)
pairWithSeparator' left sep right = FieldValue
  { fvParse = \ t ->
      let (leftChunk, rightChunk) = breakOn sep t
      in do
        x <- fvParse left leftChunk
        y <- fvParse right (drop (Data.Text.length sep) rightChunk)
        return (x, y)
  , fvEmit = \ (x, y) -> fvEmit left x <> sep <> fvEmit right y
  }

instance HasFieldValue (Int, Int) where
  fieldvalue = pairWithSeparator' number' "," number'

-- Class HasIniParser

class HasIniParser a where
    iniParser :: IniParser a

-- Class HasIniConfig

readConfig :: Maybe FilePath -> IO ConfigContent
readConfig mf = do
  cfg <- readFile =<< case mf of
                       Nothing -> getDataFileName "data/test/config_manip1.cfg"
                       Just f  -> pure f
  return $ ConfigContent $ unlines $ [fixHeader l | l <- lines cfg]
    where
      fixHeader :: Text -> Text
      fixHeader l = case findIndex (== '#' ) l of
        Nothing -> l
        Just n  -> take n l

class HasIniConfig (a :: k) where
  data Config a
  data Args a

  defaultConfig :: Config a

  getConfig :: ConfigContent
            -> Args a
            -> Capabilities
            -> Either String (Config a)

  toIni :: Config a -> Ini

  serializeConfig :: Config a -> Text
  serializeConfig = printIni . toIni

mergeHash :: (Eq k, Hashable k, Semigroup v) => HashMap k v -> HashMap k v -> HashMap k v
mergeHash = unionWith f
  where
    f :: Semigroup v => v -> v -> v
    f v1 v2 = v1 <> v2

mergeIni :: Ini -> Ini -> Ini
mergeIni x y = Ini {iniGlobals = mempty, iniSections = iniSections x `mergeHash` iniSections y}

-- Angstrom

newtype Angstrom = Angstrom { unAngstrom :: Length Double }
    deriving (Eq, Show)

instance FromJSON Angstrom where
  parseJSON = fmap (Angstrom . (*~ angstrom)) . parseJSON

instance ToJSON Angstrom where
  toJSON = toJSON . (/~ angstrom) . unAngstrom

instance Arbitrary Angstrom where
  arbitrary = Angstrom . (*~ angstrom) <$> arbitrary

instance HasFieldValue Angstrom where
  fieldvalue = FieldValue
    { fvParse =  mapRight (Angstrom . (*~ angstrom)) . fvParse auto
    , fvEmit = \(Angstrom m) -> pack . show . (/~ angstrom) $ m
    }

-- Attenuation

newtype Attenuation = Attenuation { unAttenuation :: Double }
  deriving (Eq, Show)

-- Axis

instance FieldEmitter Axis where
  fieldEmitter (Axis n t u) = fieldEmitter n <> fieldEmitter t <> fieldEmitter u

instance FieldParsable Axis where
  fieldParser = Axis
                <$> fieldParser
                <*> fieldParser
                <*> fieldParser

instance HasFieldValue Axis where
  fieldvalue = parsable

-- Capabilities

data Capabilities = Capabilities Int Int
  deriving (Eq, Show)

getCapabilities :: IO Capabilities
getCapabilities = Capabilities
                  <$> getNumCapabilities
                  <*> getNumProcessors

-- ConfigRange

newtype ConfigRange = ConfigRange (NonEmpty InputRange)
  deriving (Eq, Show)
  deriving newtype (IsList)

instance Arbitrary ConfigRange where
  arbitrary = ConfigRange <$> ((:|) <$> arbitrary <*> arbitrary)

instance FieldEmitter ConfigRange where
  fieldEmitter (ConfigRange is) = unwords . GHC.Exts.toList $ Data.List.NonEmpty.map fieldEmitter is

instance FieldParsable ConfigRange where
  fieldParser = ConfigRange <$> ((:|)
                                 <$> fieldParser <* many (satisfy isSep)
                                 <*> fieldParser `sepBy` many (satisfy isSep))
    where
      isSep :: Char -> Bool
      isSep c = c == ' ' || c == ','

instance HasFieldValue ConfigRange where
  fieldvalue = parsable

getInitialScannumber :: ConfigRange -> Scannumber
getInitialScannumber (ConfigRange ne) = let (InputRange int) = Data.List.NonEmpty.head ne
                                            sn0 = inf int
                                        in Scannumber sn0

-- DestinationTmpl

newtype DestinationTmpl =
  DestinationTmpl { unDestinationTmpl :: Text }
  deriving (Eq, Show)

instance Arbitrary DestinationTmpl where
  arbitrary = pure $ DestinationTmpl "{first}_{last}.h5"

instance HasFieldValue DestinationTmpl where
  fieldvalue = FieldValue
    { fvParse = Right . DestinationTmpl . uncomment
    , fvEmit = \(DestinationTmpl t) -> t
    }

instance FieldEmitter Double where
  fieldEmitter d = pack $ printf "%f" d

-- DynamicMask

newtype DynamicMask =
  DynamicMask { unDynamicMask :: Double }
  deriving (Eq, Show)
  deriving newtype HasFieldValue

-- FlatLocation

data FlatLocation = FlatLocation Text
                  | FlatLocation'Tmpl Text
    deriving (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance FieldEmitter FlatLocation where
  fieldEmitter (FlatLocation t)      = t
  fieldEmitter (FlatLocation'Tmpl t) = t

instance FieldParsable FlatLocation where
  fieldParser = do
    let loc :: Text -> FlatLocation
        loc t = if "{scannumber:" `Data.Text.isInfixOf` t
                then FlatLocation'Tmpl (strip t)
                else FlatLocation (strip t)

    t <- takeTill (== '|')
    if t == ""
      then fail "FlatLocation is Empty"
      else do mc <- peekChar
              case mc of
                Nothing -> pure $ loc t
                Just c  -> fail ("FlatLocation " <> [c])

instance HasFieldValue FlatLocation where
  fieldvalue = parsable

-- Geometry

iniParser'Maybe'Geometry :: IniParser (Maybe Geometry)
iniParser'Maybe'Geometry
    = do res <- sectionMb "geometry" $
               (,)
               <$> fieldOf "geometry_sample" (parseOnly words')
               <*> fieldMbOf "geometry_detector" (parseOnly words')
         case res of
           Nothing -> pure Nothing
           Just (sample, mdetector) -> do
                    sample_axes <- mapM iniParser'Axis sample
                    detector_axes <- case mdetector of
                                      Nothing       -> pure []
                                      Just detector -> mapM iniParser'Axis detector
                    pure $ mk'Geometry sample_axes detector_axes
    where
      words' :: Parser [Text]
      words' = Data.Text.words <$> takeText

      iniParser'Axis :: Text -> IniParser Axis
      iniParser'Axis n
          = do (t, u) <- Data.Ini.Config.section "geometry" $ fieldOf ("axis_" <> n) (parseOnly ((,) <$> fieldParser <*> fieldParser))
               pure $ Axis (unpack n) t u

iniParser'GeometryValues :: IniParser (Map Text Double)
iniParser'GeometryValues
    = pure Data.Map.Strict.empty

iniParser'GeometryShifts :: IniParser (Map Text Int)
iniParser'GeometryShifts
    = pure Data.Map.Strict.empty

-- HklBinocularsQCustomSubProjectionEnum

instance FieldEmitter HklBinocularsQCustomSubProjectionEnum where
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QxQyQz            = "qx_qy_qz"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QTthTimestamp     = "q_tth_timestamp"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QTimestamp        = "q_timestamp"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QparQperTimestamp = "qpar_qper_timestamp"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QparQper          = "qpar_qper"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QPhiQx            = "q_phi_qx"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QPhiQy            = "q_phi_qy"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QPhiQz            = "q_phi_qz"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QStereo           = "q_stereo"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis = "deltalab_gammalab_sampleaxis"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'XYZ                  = "x_y_z"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'YZTimestamp          = "y_z_timestamp"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QQparQper            = "q_qpar_qper"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QparsQperTimestamp   = "qpars_qper_timestamp"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QparQperSampleaxis   = "qpar_qper_sampleaxis"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QSampleaxisTth       = "q_sampleaxis_tth"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QSampleaxisTimestamp = "q_sampleaxis_timestamp"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QxQyTimestamp        = "qx_qy_timestamp"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QxQzTimestamp        = "qx_qz_timestamp"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QyQzTimestamp        = "qy_qz_timestamp"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'TthAzimuth           = "tth_azimuth"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QTimescan0           = "q_timescan0"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'QScannumber          = "q_scannumber"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'TthScannumber        = "tth_scannumber"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'PhixQThetax          = "phix_q_thetax"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'PhiyQThetay          = "phiy_q_thetay"
  fieldEmitter HklBinocularsQCustomSubProjectionEnum'PhizQThetaz          = "phiz_q_thetaz"

instance FieldParsable HklBinocularsQCustomSubProjectionEnum where
  fieldParser = go . strip . uncomment . toLower =<< takeText
    where
      err t =  "Unsupported "
               ++ show (typeRep (Proxy :: Proxy HklBinocularsQCustomSubProjectionEnum))
               ++ " :" ++ unpack t
               ++ " Supported ones are: "
               ++ unpack (unwords $ Prelude.map fieldEmitter [minBound..maxBound :: HklBinocularsQCustomSubProjectionEnum])

      go :: Text -> Parser HklBinocularsQCustomSubProjectionEnum
      go "q_index" = pure HklBinocularsQCustomSubProjectionEnum'QTimestamp
      go "angle_zaxis_omega" = pure HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis
      go "angle_zaxis_mu" = pure HklBinocularsQCustomSubProjectionEnum'DeltalabGammalabSampleaxis
      go t = case parseEnum (err t) t of
        Right p   -> pure p
        Left err' -> fail err'

instance HasFieldValue HklBinocularsQCustomSubProjectionEnum where
  fieldvalue = parsable


instance HasFieldComment HklBinocularsQCustomSubProjectionEnum where
  fieldComment _ = [ "The sub-projection that can be computed with binoculars-ng"
                   , ""
                   , "the list of the available sub-projections are:"
                   , ""
                   ]
                   <> [" - " <> fvEmit fieldvalue v | v <- [minBound..maxBound :: HklBinocularsQCustomSubProjectionEnum]]

-- HklBinocularsSurfaceOrientationEnum

instance HasFieldValue HklBinocularsSurfaceOrientationEnum where
  fieldvalue = FieldValue { fvParse = parse . strip . uncomment, fvEmit = emit }
    where
      err t = "Unsupported "
              ++ show (typeRep (Proxy :: Proxy HklBinocularsSurfaceOrientationEnum))
              ++ " :" ++ unpack t
              ++ " Supported ones are: "
              ++ unpack (unwords $ Prelude.map emit [minBound..maxBound])

      parse :: Text -> Either String HklBinocularsSurfaceOrientationEnum
      parse t = parseEnum (err t) t

      emit :: HklBinocularsSurfaceOrientationEnum -> Text
      emit HklBinocularsSurfaceOrientationEnum'Vertical   = "vertical"
      emit HklBinocularsSurfaceOrientationEnum'Horizontal = "horizontal"

instance HasFieldComment HklBinocularsSurfaceOrientationEnum where
  fieldComment _ = [ "The orientation of the surface."
                   , ""
                   , "the list of the available orientation are:"
                   , ""
                   ]
                   <> [" - " <> fvEmit fieldvalue v | v <- [minBound..maxBound :: HklBinocularsSurfaceOrientationEnum]]
                   <> [ ""
                      , "this orientation if for all the Geometry axes set to zero and correspond to"
                      , "the orientation of a vector collinear to the surface."
                      ]

-- InputRange

newtype InputRange = InputRange {unInputRange :: Interval Int }
                   deriving (Eq, Show)

instance Arbitrary InputRange where
  arbitrary = InputRange <$> oneof [ Numeric.Interval.singleton <$> arbitrary
                                   , do
                                       f <- arbitrary
                                       t <- arbitrary
                                       pure $ if f < t then f...t else t...f
                                   ]

instance FieldEmitter InputRange where
  fieldEmitter (InputRange i) = pack $ if singular i
                                       then printf "%d" (sup i)
                                       else printf "%d-%d" (inf i) (sup i)

instance FieldParsable InputRange where
  fieldParser = inputRangeFromToP <|> inputRangeP'
    where
      inputRangeFromToP :: Parser InputRange
      inputRangeFromToP =  InputRange
                           <$> ((...)
                                <$> signed decimal <* char '-'
                                <*> signed decimal)

      inputRangeP' :: Parser InputRange
      inputRangeP' = InputRange . Numeric.Interval.singleton <$> signed decimal

-- InputTmpl

newtype InputTmpl = InputTmpl { unInputTmpl :: Text }
  deriving (Eq, Show)

instance Arbitrary InputTmpl where
  arbitrary = pure $ InputTmpl "inputfiles%04.nxs"

instance HasFieldValue InputTmpl where
  fieldvalue = FieldValue
    { fvParse = Right . InputTmpl . uncomment
    , fvEmit = \(InputTmpl t) -> t
    }

-- InputType

data InputTypeDeprecated
  = SixsFlyMedVEiger
  | SixsFlyMedVS70
  | SixsFlyScanUhvGisaxsEiger
  | SixsFlyScanUhvUfxc
  deriving (Eq, Show, Enum, Bounded)

instance FieldEmitter InputTypeDeprecated where
  fieldEmitter SixsFlyMedVEiger          = "sixs:flymedveiger"
  fieldEmitter SixsFlyMedVS70            = "sixs:flymedvs70"
  fieldEmitter SixsFlyScanUhvGisaxsEiger = "sixs:gisaxuhveiger"
  fieldEmitter SixsFlyScanUhvUfxc        = "sixs:flyscanuhvufxc"

instance FieldParsable InputTypeDeprecated where
  fieldParser = go . strip . uncomment . toLower =<< takeText
    where
      err t =  "Unsupported "
               ++ show (typeRep (Proxy :: Proxy InputTypeDeprecated))
               ++ " :" ++ unpack t
               ++ " Supported ones are: "
               ++ unpack (unwords $ Prelude.map fieldEmitter [minBound..maxBound :: InputTypeDeprecated])

      go :: Text -> Parser InputTypeDeprecated
      go t = case parseEnum (err t) t of
        Right p   -> pure p
        Left err' -> fail err'

instance HasFieldValue InputTypeDeprecated where
  fieldvalue = parsable

data InputType = CristalK6C
               | Custom
               | DiffabsCirpad
               | MarsFlyscan
               | MarsSbs
               | SixsFlyMedH
               | SixsFlyMedHGisaxs
               | SixsFlyMedV
               | SixsFlyMedVGisaxs
               | SixsFlyUhv
               | SixsFlyUhvGisaxs
               | SixsSbsMedH
               | SixsSbsMedHGisaxs
               | SixsSbsMedV
               | SixsSbsMedVGisaxs
               | SixsSbsUhv
               | SixsSbsUhvGisaxs
  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary InputType where
  arbitrary = elements ([minBound .. maxBound] :: [InputType])

instance FieldEmitter InputType where
  fieldEmitter CristalK6C        = "cristal:k6c"
  fieldEmitter Custom            = "custom"
  fieldEmitter DiffabsCirpad     = "diffabs:cirpad"
  fieldEmitter MarsFlyscan       = "mars:flyscan"
  fieldEmitter MarsSbs           = "mars:sbs"
  fieldEmitter SixsFlyMedH       = "sixs:flymedh"
  fieldEmitter SixsFlyMedHGisaxs = "sixs:flymedhgisaxs"
  fieldEmitter SixsFlyMedV       = "sixs:flymedv"
  fieldEmitter SixsFlyMedVGisaxs = "sixs:flymedvgisaxs"
  fieldEmitter SixsFlyUhv        = "sixs:flyuhv"
  fieldEmitter SixsFlyUhvGisaxs  = "sixs:flyuhvgisaxs"
  fieldEmitter SixsSbsMedH       = "sixs:sbsmedh"
  fieldEmitter SixsSbsMedHGisaxs = "sixs:sbsmedhgisaxs"
  fieldEmitter SixsSbsMedV       = "sixs:sbsmedv"
  fieldEmitter SixsSbsMedVGisaxs = "sixs:sbsmedvgisaxs"
  fieldEmitter SixsSbsUhv        = "sixs:sbsuhv"
  fieldEmitter SixsSbsUhvGisaxs  = "sixs:sbsuhvgisaxs"

instance FieldParsable InputType where
  fieldParser = go . strip . uncomment . toLower =<< takeText
    where
      err t =  "Unsupported "
               ++ show (typeRep (Proxy :: Proxy InputType))
               ++ " :" ++ unpack t
               ++ " Supported ones are: "
               ++ unpack (unwords $ Prelude.map fieldEmitter [minBound..maxBound :: InputType])

      go :: Text -> Parser InputType
      go "sixs:flyscanuhv" = pure SixsFlyUhv
      go "sixs:flyscanuhv2" = pure SixsFlyUhv
      go "sixs:flyscanuhvtest" = pure SixsFlyUhv
      go "sixs:sbsmedhfixdetector" = pure SixsSbsMedHGisaxs
      go "sixs:sbsmedvfixdetector" = pure SixsSbsMedVGisaxs
      go t = case parseEnum (err t) t of
        Right p   -> pure p
        Left err' -> fail err'

instance HasFieldValue InputType where
  fieldvalue = parsable

-- Limits

data Limits = Limits (Maybe Double) (Maybe Double)
  deriving (Eq, Show)

instance Arbitrary Limits where
  arbitrary = Limits <$> arbitrary <*> arbitrary

-- MaskLocation

data MaskLocation = MaskLocation Text
                  | MaskLocation'Tmpl Text
                  | MaskLocation'Or MaskLocation MaskLocation
    deriving (Eq, Generic, Show)
    deriving anyclass (FromJSON, ToJSON)

instance FieldEmitter MaskLocation where
  fieldEmitter (MaskLocation t)      = t
  fieldEmitter (MaskLocation'Tmpl t) = t
  fieldEmitter (MaskLocation'Or l r) = fieldEmitter l <> " | " <> fieldEmitter r

instance FieldParsable MaskLocation where
  fieldParser = do
    let loc :: Text -> MaskLocation
        loc t = if "{scannumber:" `Data.Text.isInfixOf` t
                then MaskLocation'Tmpl (strip t)
                else MaskLocation (strip t)

    t <- takeTill (== '|')
    if t == ""
      then fail "MaskLocation is Empty"
      else do mc <- peekChar
              case mc of
                Nothing -> pure $ loc t
                Just '|'  -> do
                  _ <- char '|'  -- extract the '|' char
                  MaskLocation'Or (loc t) <$> fieldParser
                Just c -> fail ("MaskLocation " <> [c])

instance HasFieldValue MaskLocation where
  fieldvalue = parsable

-- Meter

newtype Meter = Meter { unMeter :: Length Double }
    deriving (Eq, Show)

instance Arbitrary Meter where
  arbitrary = Meter . (*~ meter) <$> arbitrary

instance HasFieldValue Meter where
  fieldvalue = FieldValue
    { fvParse =  mapRight (Meter . (*~ meter)) . fvParse auto
    , fvEmit = \(Meter m) -> pack . show . (/~ meter) $ m
    }

-- NCores

newtype NCores = NCores { unNCores :: Int }
    deriving (Eq, Show)

instance Arbitrary NCores where
  arbitrary = NCores <$> arbitrary

instance HasFieldValue NCores where
  fieldvalue = FieldValue
    { fvParse =  mapRight NCores . fvParse auto
    , fvEmit = \(NCores m) -> pack . show $ m
    }

-- PolarisationCorrection

newtype PolarisationCorrection =
  PolarisationCorrection { unPolarisationCorrection :: Bool }
  deriving (Eq, Show)
  deriving newtype HasFieldValue

-- ProjectionType

data ProjectionType = AnglesProjection
                    | Angles2Projection
                    | HklProjection
                    | QCustomProjection
                    | QIndexProjection
                    | QparQperProjection
                    | QxQyQzProjection
                    | RealSpaceProjection
                    | PixelsProjection
                    | TestProjection

  deriving (Eq, Show, Enum, Bounded)

instance Arbitrary ProjectionType where
  arbitrary = elements ([minBound .. maxBound] :: [ProjectionType])

instance FieldEmitter ProjectionType where
  fieldEmitter AnglesProjection    = "angles"
  fieldEmitter Angles2Projection   = "angles2"
  fieldEmitter HklProjection       = "hkl"
  fieldEmitter QCustomProjection   = "qcustom"
  fieldEmitter QIndexProjection    = "qindex"
  fieldEmitter QparQperProjection  = "qparqper"
  fieldEmitter QxQyQzProjection    = "qxqyqz"
  fieldEmitter RealSpaceProjection = "realspace"
  fieldEmitter PixelsProjection    = "pixels"
  fieldEmitter TestProjection      = "test"

instance FieldParsable ProjectionType where
  fieldParser = go . strip . uncomment . toLower =<< takeText
    where
      err t =  "Unsupported "
               ++ show (typeRep (Proxy :: Proxy ProjectionType))
               ++ " :" ++ unpack t
               ++ " Supported ones are: "
               ++ unpack (unwords $ Prelude.map fieldEmitter [minBound..maxBound :: ProjectionType])

      go :: Text -> Parser ProjectionType
      go "sixs:anglesprojection" = pure AnglesProjection
      go "sixs:angles2projection" = pure Angles2Projection
      go "sixs:qindex" = pure QIndexProjection
      go "sixs:qxqyqzprojection" = pure QxQyQzProjection
      go "sixs:qparqperprojection" = pure QparQperProjection
      go "sixs:hklprojection" = pure HklProjection
      go "sixs:realspace" = pure RealSpaceProjection
      go "sixs:pixels" = pure PixelsProjection
      go "qcustom2" = pure QCustomProjection
      go t = case parseEnum (err t) t of
        Right p   -> pure p
        Left err' -> fail err'

instance HasFieldValue ProjectionType where
  fieldvalue = parsable

instance HasFieldComment ProjectionType where
  fieldComment _ = [ "The type of projection that can be computed with binoculars-ng"
                   , ""
                   , "the list of the available projections are:"
                   , ""
                   ]
                   <> [" - " <> fvEmit fieldvalue v | v <- [minBound..maxBound :: ProjectionType]]
                   <> [ ""
                      , "Some projections can be customize using the `subprojection` parameter."
                      ]

instance HasIniParser ProjectionType where
    iniParser = sectionDef "projection" QxQyQzProjection (fieldOf "type" auto')

ms :: String
ms = "#;"

uncomment :: Text -> Text
uncomment = takeWhile (`notElem` ms)

parsable :: FieldParsable a => FieldValue a
parsable = FieldValue { fvParse = parse . strip . uncomment, fvEmit = emit }
  where
    parse ::  FieldParsable a => Text -> Either String a
    parse = parseOnly fieldParser

    emit ::  FieldParsable a => a -> Text
    emit = fieldEmitter

parseEnum :: (Bounded a, Enum a, HasFieldValue a, Typeable a)
          => String -> Text -> Either String a
parseEnum err t = maybeToRight err (find match [minBound..maxBound])
  where
    match :: HasFieldValue a => a -> Bool
    match i = toLower t == fvEmit fieldvalue i

-- Resolutions

data Resolutions sh where
  Resolutions2 :: Double -> Double -> Resolutions DIM2
  Resolutions3 :: Double -> Double -> Double -> Resolutions DIM3

deriving instance Eq (Resolutions sh)
deriving instance Show (Resolutions sh)

instance Arbitrary (Resolutions DIM2) where
  arbitrary = Resolutions2 <$> arbitrary <*> arbitrary

instance Arbitrary (Resolutions DIM3) where
  arbitrary = Resolutions3 <$> arbitrary <*> arbitrary <*> arbitrary

instance IsList (Resolutions sh) where
  type Item (Resolutions sh) = Double

  toList (Resolutions2 r1 r2)    = [r1, r2]
  toList (Resolutions3 r1 r2 r3) = [r1, r2, r3]

  fromList = undefined

instance HasFieldValue (Resolutions DIM2) where
  fieldvalue = FieldValue { fvParse = parse, fvEmit = emit }
    where
      parse :: Text -> Either String (Resolutions DIM2)
      parse t = do
        rs <- (fvParse $ listWithSeparator "," auto) t
        case rs of
          [r] -> Right (Resolutions2 r r)
          [r1, r2] -> Right (Resolutions2 r1 r2)
          _ -> Left "Need one or two resolutions values for this projection"

      emit :: Resolutions DIM2 -> Text
      emit (Resolutions2 r1 r2) = intercalate "," (Prelude.map (pack . show) [r1, r2])

instance HasFieldComment (Resolutions DIM2) where
  fieldComment _ = [ "The resolution of the bins expected for the projection's axes"
                   , ""
                   , "The expected value are:"
                   , "  - one double - same resolution for all axes."
                   , "  - one double per axis - each axis has it's own resolution."
                   , ""
                   , "the latter form use a comma to separate the values and no space is allowed."
                   ]

instance HasFieldValue (Resolutions DIM3) where
  fieldvalue = FieldValue { fvParse = parse, fvEmit = emit }
    where
      parse :: Text -> Either String (Resolutions DIM3)
      parse t = do
        rs <- (fvParse $ listWithSeparator "," auto) t
        case rs of
          [r] -> Right (Resolutions3 r r r)
          [r1, r2, r3] -> Right (Resolutions3 r1 r2 r3)
          _ -> Left "Need one or three resolutions values for this projection"

      emit :: Resolutions DIM3 -> Text
      emit (Resolutions3 r1 r2 r3) = intercalate "," (Prelude.map (pack . show) [r1, r2, r3])

instance HasFieldComment (Resolutions DIM3) where
  fieldComment _ = [ "The resolution of the bins expected for the projection's axes"
                   , ""
                   , "The expected value are:"
                   , "  - one double - same resolution for all axes."
                   , "  - one double per axis - each axis has it's own resolution."
                   , ""
                   , "the latter form use a comma to separate the values and no space is allowed."
                   ]

-- RLimits

data RLimits sh where
  Limits2 :: Limits -> Limits -> RLimits DIM2
  Limits3 :: Limits -> Limits -> Limits -> RLimits DIM3

deriving instance Eq (RLimits sh)
deriving instance Show (RLimits sh)

instance Arbitrary (RLimits DIM2) where
  arbitrary = Limits2 <$> arbitrary <*> arbitrary


instance Arbitrary (RLimits DIM3) where
  arbitrary = Limits3 <$> arbitrary <*> arbitrary <*> arbitrary


limitsP' :: Parser Limits
limitsP' = Limits
           <$> lim <* char ':'
           <*> lim
  where
    lim :: Parser (Maybe Double)
    lim = optional double

showLimit :: Maybe Double -> Text
showLimit (Just l) = pack $ printf "%f" l
showLimit Nothing  = Data.Text.empty

showLimits :: Limits -> Text
showLimits (Limits f t) = showLimit f <> Data.Text.singleton ':' <> showLimit t

instance (FieldEmitter (RLimits sh)) where
  fieldEmitter ls = Data.Text.singleton '['
                    <> intercalate "," (Prelude.map showLimits (GHC.Exts.toList ls))
                    <> Data.Text.singleton ']'

instance (FieldParsable (RLimits DIM2)) where
  fieldParser = Limits2
                <$> (char '[' *> limitsP')
                <*> (char ',' *> limitsP' <* char ']')

instance (FieldParsable (RLimits DIM3)) where
  fieldParser = Limits3
                <$> (char '[' *> limitsP')
                <*> (char ',' *> limitsP')
                <*> (char ',' *> limitsP' <* char ']')

instance HasFieldValue (RLimits DIM2) where
  fieldvalue = parsable

instance HasFieldComment (RLimits DIM2) where
  fieldComment _ = [ "The limits of the bins expected for the projection's axes"
                   , ""
                   , "Sometime it is interesting to focus on a specific region of a map."
                   , "this allows to increase the resolution and keep a memory footprint acceptable."
                   , ""
                   , "The expected value is a list of <limits>. One per axis."
                   , ""
                   , "  `[<limits>,<limits>]`"
                   , ""
                   , "<limits> has this form `<double or nothing>:<double or nothing>`"
                   , "nothing means realy nothing and in this case their is no limits."
                   , ""
                   , "example:"
                   , "  - [:1,2:3]"
                   , "  - [:,2:3]"
                   ]

instance HasFieldValue (RLimits DIM3) where
  fieldvalue = parsable

instance HasFieldComment (RLimits DIM3) where
  fieldComment _ = [ "The limits of the bins expected for the projection's axes"
                   , ""
                   , "Sometime it is interesting to focus on a specific region of a map."
                   , "this allows to increase the resolution and keep a memory footprint acceptable."
                   , ""
                   , "The expected value is a list of <limits>. One per axis."
                   , ""
                   , "  `[<limits>,<limits>,<limits>]`"
                   , ""
                   , "<limits> has this form `<double or nothing>:<double or nothing>`."
                   , "nothing means realy nothing and in this case their is no limits."
                   , ""
                   , "example:"
                   , "  - [:1,2:3,4:5]"
                   , "  - [:,2:3,4:]"
                   ]

instance IsList (RLimits sh) where
  type Item (RLimits sh) = Limits

  toList (Limits2 l1 l2)    = [l1, l2]
  toList (Limits3 l1 l2 l3) = [l1, l2, l3]

  fromList = undefined

-- SampleAxis

newtype SampleAxis = SampleAxis { unSampleAxis :: Text }
  deriving (Eq, Show)

instance HasFieldValue SampleAxis where
  fieldvalue = FieldValue { fvParse = Right . SampleAxis . uncomment
                          , fvEmit = \(SampleAxis t) -> t
                          }

instance Arbitrary SampleAxis where
  arbitrary = pure $ SampleAxis "omega"

-- String

instance FieldEmitter String where
  fieldEmitter = pack

instance FieldParsable String where
  fieldParser = unpack <$> takeText

-- Transformation

instance FieldEmitter Transformation where
  fieldEmitter NoTransformation = "no-transformation"
  fieldEmitter (Rotation x y z) = "rotation " <> fieldEmitter x <> " " <> fieldEmitter y <> " " <> fieldEmitter z
  fieldEmitter (Translation x y z) = "translation " <> fieldEmitter x <> " " <> fieldEmitter y <> " " <> fieldEmitter z

instance FieldParsable Transformation where
    fieldParser = do
      t <- skipSpace *> word
      case t of
        "no-transformation" -> pure NoTransformation
        "rotation" -> Rotation
                     <$> (skipSpace *> double)
                     <*> (skipSpace *> double)
                     <*> (skipSpace *> double)
                     <?> "rotation maformed expect: 'rotation <x> <y> <z>'"
        "translation" -> Translation
                     <$> (skipSpace *> double)
                     <*> (skipSpace *> double)
                     <*> (skipSpace *> double)
                     <?> "translation maformed expect: 'translation <x> <y> <z>'"
        _ -> fail ("unknown transformation: " <> unpack t <> " only 'no-transformation', 'rotation', 'translation'")

-- Unit

instance FieldEmitter Unit where
  fieldEmitter Unit'NoUnit            = "ua"
  fieldEmitter Unit'Angle'Degree      = "degree"
  fieldEmitter Unit'Length'MilliMeter = "millimeter"

instance FieldParsable Unit where
    fieldParser = do
      t <- skipSpace *> word
      let units = [minBound..maxBound] :: [Unit]
      let supported = [ fieldEmitter u | u <- units ]
      let mi = elemIndex t supported
      case mi of
        Just i -> pure $ units !! i
        Nothing -> fail $ unpack ("unknown unit '" <> t <> "' only supported are "
                                 <> unwords [ "\'" <> t' <> "\'" | t' <- supported ])

-- BinocularsPreConfig

newtype BinocularsPreConfig =
  BinocularsPreConfig { _binocularsPreConfigProjectionType :: ProjectionType }
                         deriving (Eq, Show)

instance HasIniParser BinocularsPreConfig where
    iniParser = BinocularsPreConfig <$> iniParser

---------------
-- functions --
---------------

destination' :: ProjectionType -> Maybe HklBinocularsQCustomSubProjectionEnum -> ConfigRange -> Maybe (RLimits a) -> DestinationTmpl -> Bool -> IO FilePath
destination' proj msub (ConfigRange rs) ml dtmpl overwrite =
  if overwrite
  then pure $ replace' proj msub interval limits dtmpl Nothing
  else do
    let guess = replace' proj msub interval limits dtmpl Nothing : Prelude.map (replace' proj msub interval limits dtmpl . Just) [2..]
    findFirst guess
      where
        findFirst :: [FilePath] -> IO FilePath
        findFirst [] = undefined -- can not append non empty list
        findFirst (x : xs) = do
          exists <- doesPathExist x
          if exists
            then findFirst xs
            else return x

        interval = foldl' hull Numeric.Interval.empty intervals

        limits = maybe "nolimits" fieldEmitter ml

        intervals = Data.List.NonEmpty.map unInputRange rs

isHdf5 :: Path Abs File -> Bool
isHdf5 p =
  case (fileExtension p :: Maybe [Char]) of
    Nothing  -> False
    Just ext -> ext == ".nxs"

-- isInConfigRange :: Maybe InputTmpl -> Maybe ConfigRange -> FilePath -> Bool
-- isInConfigRange mtmpl mr f
--   = case mr of
--       (Just (ConfigRange rs)) -> do
--         let tmpl = maybe "%05d" (unpack . unInputTmpl) mtmpl
--         any (isInInputRange (filename f) tmpl) rs
--       Nothing -> True
--   where
--     matchIndex :: Path Rel File -> String -> Int -> Bool
--     matchIndex p tmpl n = printf tmpl n `isInfixOf` toFilePath p

--     isInInputRange :: Path Rel File -> String -> InputRange -> Bool
--     isInInputRange p tmpl (InputRange i) = any (matchIndex p tmpl) [inf i .. sup i]

--                  return $ filter (\f -> all ($ f) filters) sfs)

matchInputRange' :: String -> Path Abs File -> [Int] -> [Maybe ScanFilePath]
matchInputRange' tmpl f is =
  case find (\i -> printf tmpl i `Data.List.isInfixOf` toFilePath f) is of
    Nothing -> []
    Just i  -> [Just (ScanFilePath f (Scannumber i))]

matchInputRange :: String -> Path Abs File ->  InputRange -> [Maybe ScanFilePath]
matchInputRange tmpl f (InputRange ir)
  = matchInputRange' tmpl f [inf ir .. sup ir]

matchConfigRange' :: String -> ConfigRange -> Path Abs File -> [Maybe ScanFilePath]
matchConfigRange' tmpl (ConfigRange is) f = concatMap (matchInputRange tmpl f) is

matchConfigRange :: Maybe InputTmpl -> ConfigRange -> [Path Abs File] -> [Maybe ScanFilePath]
matchConfigRange mt r fs =
  let tmpl = maybe "%05d" (unpack . unInputTmpl) mt
  in concatMap (matchConfigRange' tmpl r) fs

files :: (MonadThrow m, MonadIO m)
       => Maybe (Path Abs Dir)
       -> ConfigRange
       -> Maybe InputTmpl
       -> m [ScanFilePath]
files md mr mt =
  do dir <- maybe getCurrentDir pure md

     fs <- walkDirAccum Nothing
          (\_root _dirs fs ->
              do let fss = filter isHdf5 fs
                 return $ matchConfigRange mt mr fss)
          dir

     if null fs
       then throwM (NoDataFilesUnderTheGivenDirectory dir)
       else return $ catMaybes fs

getFlat :: (MonadCatch m, MonadIO m) => FlatLocation -> Detector Hkl DIM2 -> Scannumber -> m Flat
getFlat loc d (Scannumber i)
  = case loc of
      FlatLocation "default" -> getDetectorDefaultFlat d
      FlatLocation fname     -> getDetectorFlat d fname
      FlatLocation'Tmpl tmpl -> do
        let tmpl1 = replace "{scannumber:" "%" tmpl
        let tmpl2 = replace "}" "" tmpl1
        let fname = pack $ printf (unpack tmpl2) i
        getDetectorFlat d fname

getMask :: (MonadCatch m, MonadIO m) => MaskLocation -> Detector Hkl DIM2 -> Scannumber -> m Mask
getMask loc d sn@(Scannumber i)
  = case loc of
      MaskLocation "default" -> getDetectorDefaultMask d
      MaskLocation fname     -> getDetectorMask d fname
      MaskLocation'Tmpl tmpl -> do
        let tmpl1 = replace "{scannumber:" "%" tmpl
        let tmpl2 = replace "}" "" tmpl1
        let fname = pack $ printf (unpack tmpl2) i
        getDetectorMask d fname
      MaskLocation'Or l r -> do
        el :: Either HklDetectorException Mask <- try $ getMask l d sn
        case el of
          Left _ -> getMask r d sn
          Right ml -> do
            er :: Either HklDetectorException Mask <- try $ getMask r d sn
            case er of
              Left _   -> pure ml
              Right mr -> maskOr d ml mr


getPreConfig' :: ConfigContent -> Either String BinocularsPreConfig
getPreConfig' (ConfigContent cfg) = parseIniFile cfg iniParser

getPreConfig :: Maybe FilePath -> IO (Either String BinocularsPreConfig)
getPreConfig mf = getPreConfig' <$> readConfig mf

addOverwrite :: Maybe Int -> DestinationTmpl -> DestinationTmpl
addOverwrite midx tmpl = case midx of
  Nothing -> tmpl
  Just idx -> let (f, ext) = splitExtensions . unpack . unDestinationTmpl $ tmpl
             in DestinationTmpl (pack $ f <> printf "_%02d" idx <> ext)

replace' :: ProjectionType -> Maybe HklBinocularsQCustomSubProjectionEnum -> Interval Int -> Text -> DestinationTmpl -> Maybe Int -> FilePath
replace' proj msub i l dtmpl midx = unpack
                          . replace "{last}" (pack . show . sup $ i)
                          . replace "{first}" (pack . show . inf $ i)
                          . replace "{limits}" l
                          . replace "{projection}" (case msub of
                                                      Just sub -> fieldEmitter sub
                                                      Nothing -> fieldEmitter proj)
                          . unDestinationTmpl . addOverwrite midx $ dtmpl


-- geometry_sample = pitch, mu
-- geometry_detector = pitch, gamma, delta

-- axis_pitch = rotation 0 -1 0 deg
-- axis_mu = rotation 0 0 1 deg
-- axis_gamma = rotation 0 0 1 deg
-- axis_delta = rotation 0 -1 0 deg


-- parse'Axis :: Text -> IniParser Axis
-- parse'Axis n
--   = Data.Ini.Config.section "geometry" $ do
--   fieldOf ("axis_" <> n) auto'

-- parse'Geometry :: IniParser (Maybe Geometry)
-- parse'Geometry
--   = do
--   msamples <-  Data.Ini.Config.section "geometry" $ fieldOf "geometry_sample" auto'
--   mdetectors <-  Data.Ini.Config.section "geometry" $ fieldOf "geometry_detector" auto'
--   axes_samples <- mapM (\a -> parse'Axis a) msamples
--   axes_detectors <- mapM (\a -> parse'Axis a) mdetectors
--   let mg = mk'Geometry axes_samples axes_detectors
--   pure $ mk'Geometry axes_samples axes_detectors

------------------
-- Parser utils --
------------------

parseError :: Text -> Text -> String -> Either String r
parseError s f err = Left ("Can not parse " <> unpack s <> "." <> unpack f <> ": " <> err)

parse' :: HasFieldValue b => Text -> Text -> Text -> Either String (Maybe b)
parse' c s f = parseIniFile c $ Data.Ini.Config.section s (fieldMbOf f auto')

eitherF :: (t1 -> p) -> Either t1 t2 -> (t2 -> p) -> p
eitherF fa (Left a)  _ = fa a
eitherF _ (Right b) fb = fb b

parseF :: HasFieldValue b
       => Text -> Text -> Text -> (Maybe b -> Either String r) -> Either String r
parseF c s f = eitherF (parseError s f) (parse' c s f)

parseFDef :: HasFieldValue p
          => Text -> Text -> Text -> p -> Either String p
parseFDef c s f def = parseF c s f $ \case
  Nothing -> Right def
  Just b  -> Right b

parseMb ::  HasFieldValue r
        => Text -> Text -> Text -> Either String (Maybe r)
parseMb c s f = eitherF (parseError s f) (parse' c s f) pure

parseMbDef :: HasFieldValue r
           => Text -> Text -> Text -> Maybe r -> Maybe r
parseMbDef c s f def = eitherF error (parse' c s f) (<|> def)

-----------------
-- Ini Emitter --
-----------------

elemFDef' :: HasFieldComment w => Text -> (v -> w) -> v -> v -> [(Text, Text)]
elemFDef' k f v d = elemFDef k f v d (fieldComment (f v))


elemContent :: Text -> Text -> [Text] -> [Text]
elemContent k def cs =
  [ ""
  , k <> ":"
  , ""
  ]
  <> cs
  <> [ ""
     , "default value: `" <> def <> "`"
     , ""
     , "uncomment and edit the next line if you want to modify the value"
     ]

elemFDef ::  HasFieldValue w => Text -> (v -> w) -> v -> v -> [Text] -> [(Text, Text)]
elemFDef k f v d cs = [ ("#", l) | l <- ls ] <> [(k,  fvEmit fieldvalue (f v))]
  where
    ls :: [Text]
    ls = elemContent k defv cs

    defv :: Text
    defv = fvEmit fieldvalue (f d)

elemFMbDef' ::  HasFieldComment w => Text -> (v -> Maybe w) -> v -> v -> [(Text, Text)]
elemFMbDef' k f v d = elemFMbDef k f v d (fieldComment (fromMaybe undefined (f v)))

elemFMbDef ::  HasFieldValue w => Text -> (v -> Maybe w) -> v -> v -> [Text] -> [(Text, Text)]
elemFMbDef k f v d cs = [ ("#", l) | l <- ls ] <> maybe [("# " <> k, "")] (\w -> [(k, fvEmit fieldvalue w)]) (f v)
  where
    ls :: [Text]
    ls = elemContent k defv cs

    defv :: Text
    defv = case f d of
             Nothing -> "<not set>"
             Just w  -> fvEmit fieldvalue w
